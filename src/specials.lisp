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
                (llvm:linkage llvm-value) :internal
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
        (dolist (export (exports import))
          (let ((remote-binding (lookup export (env import))))
            (when (and (typep remote-binding 'var)
                       (not (typep (var-type remote-binding) 'universal-type)))
              (let ((val (if (ftype? (var-type remote-binding))
                             (prog1-let (func (llvm:add-function
                                               lmodule
                                               (symbol-fqn import export)
                                               (llvm (var-type remote-binding))))
                               (setf (llvm:function-calling-convention func) :fast))
                             (llvm:add-global lmodule
                                              (llvm (var-type remote-binding))
                                              (symbol-fqn import export)))))
                (setf (llvm:linkage val) :external
                      (llvm remote-binding) val))))))))

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
                (llvm:build-ret builder (llvm:build-call builder cfunc (llvm:params blfunc) ""))))))
