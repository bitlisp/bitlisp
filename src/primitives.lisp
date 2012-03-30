(in-package #:bitlisp)

(defmacro defspecial (name self args
                      (env &body resolver)
                      (vargen &body constrainer)
                      (unifier utype &body subst)
                      (module builder ctype &body codegen))
  `(let ((,self (make-special-op :name ,name)))
     (setf (special-op-resolver ,self) (lambda (,env ,@args) ,@resolver)
           (special-op-constrainer ,self) (lambda (,vargen ,@args) ,@constrainer)
           (special-op-subst ,self) (lambda (,unifier ,utype ,@args) ,@subst)
           (special-op-codegen ,self) (lambda (,module ,builder ,ctype ,@args) ,@codegen))
     (bind *core-env* (make-bl-symbol ,name) ,self)))

(defspecial "nlambda" self (name args &rest body)
    (env
      (let ((new-env (make-env :parents (list env) :toplevel? nil))
            (name-var (make-instance 'var :name name))
            (arg-vars (mapcar (lambda (sym) (make-instance 'var :name sym)) args)))
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
                                         (form-type (car (last forms)))
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
      (prog1-let* ((func (llvm:add-function module "lambda" (llvm type)))
                   (entry (llvm:append-basic-block func "entry")))
        (setf (llvm name) func)
        (mapc (lambda (param var) (setf (llvm var) param))
              (llvm:params func) args)
        (llvm:with-builder (local-builder)
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
      (let ((var (make-instance 'var :name name)))
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
        (make-form final-type
                   (list self name (make-form final-type (form-code value-form))))))
    (module builder type
      ;; TODO: Nontrivial values
      (declare (ignore type))
      (setf (llvm name) (codegen module builder value))))

(defmacro def-bl-type (name class &rest initargs)
  `(bind *core-env* (make-bl-symbol ,name) (make-instance ',class ,@initargs)))

(def-bl-type "Unit" unit-type)
(def-bl-type "Float"  simple-type :llvm (llvm:float-type))
(def-bl-type "Double" simple-type :llvm (llvm:double-type))
;;; TODO: 64-bit support
(def-bl-type "Word"  machine-int :bits 32 :signed t)
(def-bl-type "UWord" machine-int :bits 32 :signed nil)
(def-bl-type "Byte"  machine-int :bits 8  :signed t)
(def-bl-type "UByte" machine-int :bits 8  :signed nil)
(def-bl-type "Bool"  machine-int :bits 1  :signed nil)

(defparameter *primfun-builders* nil)

(defun build-primfuns (module)
  (mapc (lambda (pair)
          (setf (llvm (car pair)) (funcall (cdr pair) module)))
        *primfun-builders*))

(defmacro defprimfun (name rtype args (module builder type) &body llvm)
  (with-gensyms (symbol func entry var)
   `(let* ((,symbol (make-bl-symbol ,name))
           (,type (make-ftype (lookup ,rtype) ,@(mapcar (compose (curry #'list 'lookup) #'second) args)))
           (,var (make-instance 'var
                                :name ,symbol
                                :var-type ,type)))
      (push (cons ,var (lambda (,module)
                         (prog1-let* ((,func (llvm:add-function module ,name (llvm ,type)))
                                      (,entry (llvm:append-basic-block ,func "entry")))
                           (llvm:with-builder (,builder)
                             (llvm:position-builder-at-end builder ,entry)
                             (destructuring-bind ,(mapcar #'first args)
                                 (llvm:params ,func)
                               ,@llvm)))))
            *primfun-builders*)
      (bind *core-env* ,symbol ,var))))

(defprimfun "word+" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-add builder lhs rhs "result")))

(defprimfun "word-" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-sub builder lhs rhs "result")))

(defprimfun "word>" "Bool" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-i-cmp builder := lhs rhs "result")))

;; (defprimfun "load" (make-instance 'universal-type
;;                                   :variables '(0)
;;                                   :inner-type (make-ftype 0 (make-ptr 0))))

;; (defprimfun "store" (make-instance 'universal-type
;;                                    :variables '(0)
;;                                    :inner-type (make-ftype 0 (make-ptr 0))))
