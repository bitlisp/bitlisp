(in-package #:bitlisp)

(defmacro defspecial (name self (&rest args)
                      (env &body resolver)
                      &optional
                        ((vargen &body constrainer) nil crsupplied?)
                        ((unifier utype &body subst) '(nil nil) ufsupplied?)
                        ((module builder ctype &body codegen) '(nil nil nil) cgsupplied?))
  (let ((errlambda `(lambda (&rest args)
                      (declare (ignore args))
                      (error ,(concatenate 'string
                                           "Internal error: Special op " name
                                           " present at invalid stage!")))))
   `(let ((,self (make-special-op :name ,name)))
      (setf (special-op-resolver ,self) (lambda (,env ,@args) ,@resolver)
            (special-op-constrainer ,self) ,(if crsupplied?
                                                `(lambda (,vargen ,@args)
                                                   ,@constrainer)
                                                errlambda)
            (special-op-subst ,self) ,(if ufsupplied?
                                          `(lambda (,unifier ,utype ,@args)
                                             ,@subst)
                                          errlambda)
            (special-op-codegen ,self) ,(if cgsupplied?
                                            `(lambda (,module ,builder ,ctype ,@args)
                                               ,@codegen)
                                            errlambda))
      (bind *primitives-env* (make-bl-symbol ,name) ,self))))

(defspecial "nlambda" self (name args &rest body)
    (env
      (let* ((new-env (make-instance 'environment
                                     :parents (list env)
                                     :toplevel? nil
                                     :module (module env)))
             (name-var (make-instance 'var :name name :env new-env))
             (arg-vars (mapcar (lambda (sym)
                                 (make-instance 'var :name sym :env new-env))
                               args)))
        (bind new-env name name-var)
        (mapc (curry #'bind new-env) args arg-vars)
        (list* self name-var arg-vars
               (mapcar (curry #'resolve new-env) body))))
    (vargen
     ;; TODO: Probably shouldn't mutate here.
      (setf (var-type name) (funcall vargen))
      (mapc (lambda (v) (setf (var-type v) (funcall vargen))) args)
      (loop :with constraints := nil
            :with forms := nil
            :for form :in body
            :do (multiple-value-bind (new-form new-constraints)
                    (constrain vargen form)
                  (push new-form forms)
                  (push new-constraints constraints))
            :finally (let ((ftype (apply #'make-ftype
                                         (form-type (first forms))
                                         (mapcar #'var-type args))))
                       (return
                         (values
                          (make-form ftype
                                     (list* self name args (nreverse forms)))
                          (apply #'nconc (list (cons ftype (var-type name))) constraints))))))
    (unifier type
      (setf (var-type name) (subst-apply unifier (var-type name)))
      (mapc (lambda (v) (setf (var-type v) (subst-apply unifier (var-type v)))) args)
      (make-form (subst-apply unifier type)
                 (list* self name args (mapcar (curry 'unif-apply unifier) body))))
    (module builder type
      (declare (ignore builder))
      (prog1-let* ((func (llvm:add-function module (var-fqn name) (llvm type)))
                   (entry (llvm:append-basic-block func "entry")))
        (setf (llvm:linkage func) :internal
              (llvm:function-calling-convention func) :fast
              (llvm name) func)
        (mapc (lambda (param var) (setf (llvm var) param
                                        (llvm:value-name param) (name (name var))))
              (llvm:params func) args)
        (llvm:with-object (local-builder builder)
          (llvm:position-builder-at-end local-builder entry)
          (loop :for (form . rest) :on body
                :for genned := (codegen module local-builder form)
                :unless rest :do (llvm:build-ret local-builder genned))))))

(defspecial "if" self (condition then else)
    (env
      (list self (resolve env condition)
            (resolve env then) (resolve env else)))
    (vargen
      (multiple-value-bind (cform ccons) (constrain vargen condition)
        (multiple-value-bind (tform tcons) (constrain vargen then)
          (multiple-value-bind (eform econs) (constrain vargen else)
            (values (make-form (form-type tform) (list self cform tform eform))
                    (nconc (list (cons (form-type tform)
                                       (form-type eform)))
                           ccons tcons econs))))))
    (unifier type
      (make-form (subst-apply unifier type)
                 (list self
                       (unif-apply unifier condition)
                       (unif-apply unifier then)
                       (unif-apply unifier else))))
    (module builder type
      (let* ((cond-result (codegen module builder condition))
             (func (llvm:basic-block-parent (llvm:insertion-block builder)))
             (then-block (llvm:append-basic-block func "then"))
             (else-block (llvm:append-basic-block func "else"))
             (done-block (llvm:append-basic-block func "endif"))
             then-result else-result)
        (llvm:build-cond-br builder cond-result then-block else-block)

        (llvm:position-builder-at-end builder then-block)
        (setf then-result (codegen module builder then)
              then-block (llvm:insertion-block builder))
        (llvm:build-br builder done-block)

        (llvm:position-builder-at-end builder else-block)
        (setf else-result (codegen module builder else)
              else-block (llvm:insertion-block builder))
        (llvm:build-br builder done-block)

        (llvm:position-builder-at-end builder done-block)
        (prog1-let (phi (llvm:build-phi builder (llvm type) "result"))
          (llvm:add-incoming phi
                             (list then-result else-result)
                             (list then-block  else-block))))))

(defspecial "def" self (name value)
    (env
      (let ((var (make-instance 'var :name name :env env)))
        (bind env name var)
        (list self var (resolve env value))))
    (vargen
      (multiple-value-bind (vform vcons)
          (constrain vargen value)
        (values
         (make-form (form-type vform) (list self name vform))
         vcons)))
    (unifier type
      (let ((final-type (generalize-type (subst-apply unifier type)))
            (value-form (unif-apply unifier value)))
        (setf (var-type name) final-type)
        (when (typep final-type 'universal-type)
          (setf (instantiator name) (lambda (llvm-module &rest vars)
                                      (codegen llvm-module nil
                                               (apply #'instantiate-value
                                                      final-type (form-code value-form) vars)))))
        (make-form final-type
                   (list self name (make-form final-type (form-code value-form))))))
    (module builder type
      ;; TODO: Non-function values
      (unless (typep type 'universal-type)
        (let ((llvm-value (codegen module builder value)))
          (setf (llvm:value-name llvm-value) (var-fqn name)
                (llvm:linkage llvm-value) :external
                (llvm name) llvm-value)))))

(defspecial "module" self (name imports &rest exports)
    (env
      (assert (toplevel? env) () "Cannot change modules below the top level")
      (let ((import-modules (mapcar (rcurry #'lookup env) imports)))
        (let ((module (make-module name (module env)
                                   import-modules
                                   exports)))
          (values (list* self name import-modules exports)
                  module))))
    (vargen
      (declare (ignore vargen))
      (make-form (lookup "Unit") (list* self name imports exports)))
    (unifier utype
      (declare (ignore unifier))
      (make-form utype (list* self name imports exports)))
    (lmodule builder type
      (declare (ignore builder name exports type))
      (dolist (import imports)
        (maphash
         (lambda (binding-name var)
           (when (and (typep var 'var)
                      (not (typep (var-type var) 'universal-type)))
             (let ((val (if (ftype? (var-type var))
                            (prog1-let (func (llvm:add-function
                                              lmodule
                                              (symbol-fqn import binding-name)
                                              (llvm (var-type var))))
                              (setf (llvm:function-calling-convention func) :fast))
                            (llvm:add-global lmodule
                                             (llvm (var-type var))
                                             (symbol-fqn import binding-name)))))
               (setf (llvm:linkage val) :external
                     (llvm var) val))))
         (bindings (env import))))))

(defspecial "the" self (type value)
    (env (list self (type-eval type env) (resolve env value)))
    (vargen
      (multiple-value-bind (value-form constraints) (constrain vargen value)
        (values value-form
                (cons (cons type (form-type value-form))
                      constraints)))))

(defspecial "cdef" self (name return-type &rest args)
    (env
      (let* ((rtype (type-eval return-type env))
             (typed-args (mapcar (lambda (arg)
                                   (list (first arg) (type-eval (second arg) env)))
                                 args))
             (var (make-instance 'var
                                 :name name
                                 :env env
                                 :var-type (apply #'make-ftype rtype
                                                  (mapcar #'second
                                                          typed-args)))))
        (bind env name var)
        (list* self var rtype typed-args)))
    (vargen
     (declare (ignore vargen))
     (make-form (lookup "Unit") (list* self name return-type args)))
    (unifier utype
             (declare (ignore unifier))
             (make-form utype (list* self name return-type args)))
    (module builder ctype
            (declare (ignore builder ctype return-type args))
            (let* ((cfunc (llvm:add-function module (name (name name)) (llvm (var-type name))))
                   (blfunc (llvm:add-function module (var-fqn name) (llvm (var-type name))))
                   (entry (llvm:append-basic-block blfunc "entry")))
              (llvm:add-function-attributes blfunc :inline-hint)
              (setf (llvm:function-calling-convention blfunc) :fast
                    (llvm name) blfunc)
              (llvm:with-object (builder builder)
                (llvm:position-builder-at-end builder entry)
                (llvm:build-ret builder (llvm:build-call builder cfunc (llvm:params blfunc) "result"))))))

(defmacro defctor (name (&rest args) &body builder)
  (with-gensyms (sym)
   `(let ((,sym (make-bl-symbol ,name)))
      (bind *primitives-env* ,sym
            (make-instance 'type-constructor
                           :name ,sym
                           :llvm (lambda ,args ,@builder))))))

(defctor "Int" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "UInt" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "Func" (return-type &rest arg-types)
  (llvm:function-type (llvm return-type) (mapcar #'llvm arg-types)))
(defctor "Ptr" (inner-type)
  (llvm:pointer-type (llvm inner-type)))
(defctor "Vector" (inner-type count)
  (check-type count integer)
  (assert (or (eq inner-type (lookup "Bool"))
              (and (typep inner-type 'constructed-type)
                   (member (constructor inner-type)
                           (list (lookup "Int")
                                 (lookup "UInt")
                                 (lookup "Ptr")
                                 (lookup "Float")
                                 (lookup "Double")))))
          (inner-type)
          "Constructing vectors of type ~A is unsupported" inner-type)
  (llvm:vector-type (llvm inner-type) count))

(defun make-ftype (return-type &rest arg-types)
  (make-instance 'constructed-type
                 :constructor (lookup "Func")
                 :args (list* return-type arg-types)))

(defun ftype? (type)
  (or (and (typep type 'constructed-type)
           (eq (lookup "Func") (constructor type)))
      (and (typep type 'universal-type)
           (ftype? (inner-type type)))))

(defun make-ptr (target-type)
  (make-instance 'constructed-type
                 :constructor (lookup "Ptr")
                 :args (list target-type)))

(defmacro defsimpletype (name llvm)
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* ,sym
             (make-instance 'simple-type
                            :name ,sym
                            :llvm ,llvm)))))
(defsimpletype "Unit"   (llvm:void-type))
(defsimpletype "Float"  (llvm:float-type))
(defsimpletype "Double" (llvm:double-type))
(defsimpletype "Bool"   (llvm:int1-type))

(defmacro defint (name width signed)
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* ,sym
             (make-instance 'constructed-type
                            :constructor ,(if signed
                                              '(lookup "Int")
                                              '(lookup "UInt"))
                            :args '(,width))))))
;;; TODO: Architecture portability
(defint "Word"  32 t)
(defint "UWord" 32 nil)
(defint "Byte"  8  t)
(defint "UByte" 8  nil)

(defparameter *primfun-builders* nil)

(defun build-primfuns (bl-module llvm-module)
  (mapc (lambda (pair)
          (setf (llvm (car pair)) (funcall (cdr pair) bl-module llvm-module)))
        *primfun-builders*))

(defmacro defprimfun (name rtype args (module builder type) &body llvm)
  (with-gensyms (symbol func entry var bl-module)
   `(let* ((,symbol (make-bl-symbol ,name))
           (,type (make-ftype (lookup ,rtype) ,@(mapcar (compose (curry #'list 'lookup) #'second) args)))
           (,var (make-instance 'var
                                :name ,symbol
                                :var-type ,type)))
      (push (cons ,var (lambda (,bl-module ,module)
                         (prog1-let* ((,func (llvm:add-function module
                                                                (symbol-fqn ,bl-module
                                                                            (make-bl-symbol ,name))
                                                                (llvm ,type)))
                                      (,entry (llvm:append-basic-block ,func "entry")))
                           (setf (llvm:function-calling-convention ,func) :fast)
                           (mapc #'(setf llvm:value-name)
                                 ',(mapcar (compose #'string-downcase #'first) args)
                                 (llvm:params ,func))
                           (llvm:with-object (,builder builder)
                             (llvm:position-builder-at-end builder ,entry)
                             (destructuring-bind ,(mapcar #'first args)
                                 (llvm:params ,func)
                               ,@llvm)))))
            *primfun-builders*)
      (bind *primitives-env* ,symbol ,var))))

(defprimfun "word+" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-add builder lhs rhs "sum")))

(defprimfun "word-" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-sub builder lhs rhs "difference")))

(defprimfun "word>" "Bool" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-i-cmp builder :> lhs rhs "greater")))

(defmacro defprimpoly (name vars return-type args (builder) &body instantiator)
  (with-gensyms (module sym type qvars subenv entry func)
    `(let* ((,sym (make-bl-symbol ,name))
            (,qvars (mapcar (lambda (n)
                              (make-instance 'quant-var :name (make-bl-symbol n)))
                            ',(mapcar #'string-downcase vars)))
            (,type (let ((,subenv (make-subenv *primitives-env*)))
                     (mapc (curry #'bind ,subenv)
                           (mapcar #'name ,qvars)
                           ,qvars)
                     (destructuring-bind ,vars (mapcar #'name ,qvars)
                       (make-instance 'universal-type
                                      :variables ,qvars
                                      :inner-type (type-eval (list "Func" ,return-type
                                                                   ,@(mapcar #'second args))
                                                             ,subenv))))))
       (bind *primitives-env* ,sym
             (make-instance 'var
                            :var-type ,type
                            :env *primitives-env*
                            :name ,sym
                            :instantiator
                            (lambda (,module ,@vars)
                              (prog1-let* ((,func (llvm:add-function
                                                   ,module
                                                   (concatenate 'string
                                                                ;; FIXME: Hardcoded module path
                                                                ":core:" ,name
                                                                (format nil "~{!~A~}"
                                                                        (list ,@vars)))
                                                   (llvm (concretify-type ,type ,@vars))))
                                           (,entry (llvm:append-basic-block ,func "entry")))
                                (setf (llvm:function-calling-convention ,func) :fast
                                      (llvm:linkage ,func) :link-once-odr)
                                (mapc #'(setf llvm:value-name)
                                      ',(mapcar (compose #'string-downcase #'first) args)
                                      (llvm:params ,func))
                                (llvm:with-object (,builder builder)
                                  (llvm:position-builder-at-end builder ,entry)
                                  (destructuring-bind ,(mapcar #'first args)
                                      (llvm:params ,func)
                                    ,@instantiator)))))))))

(defprimpoly "load" (a) a ((pointer `("Ptr" ,a))) (builder)
  (declare (ignore a))
  (llvm:build-ret builder (llvm:build-load builder pointer "value")))
(defprimpoly "store" (a) a ((pointer `("Ptr" ,a)) (value a)) (builder)
  (declare (ignore a))
  (llvm:build-store builder value pointer)
  (llvm:build-ret builder value))
