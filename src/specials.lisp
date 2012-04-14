(in-package #:bitlisp)

(defmacro defspecial (name self (&rest args)
                      (env &body resolver)
                      (&body inferrer)
                      (module builder ctype &body codegen))
  `(let ((,self (make-special-op :name ,name)))
     (setf (special-op-resolver ,self) (lambda (,env ,@args) ,@resolver)
           (special-op-inferrer ,self) (lambda (,@args)
                                         ,@inferrer)
           (special-op-codegen ,self) (lambda (,module ,builder ,ctype ,@args)
                                        ,@codegen))
     (bind *primitives-env* :value (make-bl-symbol ,name) ,self)))

(defspecial "lambda" self (args &rest body)
    (env
      (let* ((new-env (make-instance 'environment
                                     :parents (list env)
                                     :toplevel? nil
                                     :module (module env)))
             (arg-vars (mapcar (lambda (sym)
                                 (make-instance 'var :name sym :env new-env))
                               args)))
        (mapc (curry #'bind new-env :value) args arg-vars)
        (list* self arg-vars
               (mapcar (rcurry #'resolve new-env) body))))
    (;; TODO: Probably shouldn't mutate here.
     (let ((arg-types (loop :repeat (length args) :collect (make-instance 'tyvar :kind 1))))
       (mapc (lambda (var ty) (setf (var-type var) (to-scheme ty)))
             args arg-types)
       (multiple-value-bind (forms preds subst) (infer-expr-seq body)
         (values (make-form (make-ftype (apply #'make-prodty arg-types)
                                        (form-type (lastcar forms)))
                            (list* self args forms))
                 preds
                 subst))))
    (module builder type
      (declare (ignore builder))
      (prog1-let (func (llvm:add-function module "lambda" (llvm type)))
        (setf (llvm:linkage func) :internal
              (llvm:function-calling-convention func) :fast)
        (mapc (lambda (param var) (setf (llvm var) param
                                        (llvm:value-name param) (name (name var))))
              (llvm:params func) args)
        (llvm:with-object (local-builder builder)
          (llvm:position-builder-at-end local-builder
                                        (llvm:append-basic-block func "entry"))
          (loop :for (form . rest) :on body
                :for genned := (codegen module local-builder form)
                :unless rest :do (llvm:build-ret local-builder genned))))))

(defspecial "if" self (condition then else)
    (env
      (list self (resolve condition env)
            (resolve then env) (resolve else env)))
    ((multiple-value-bind (cform cpreds csubst) (infer-expr condition)
       (multiple-value-bind (tform tpreds tsubst) (infer-expr then)
         (multiple-value-bind (eform epreds esubst) (infer-expr else)
           (values (make-form (form-type tform) (list self cform tform eform))
                   (nconc cpreds tpreds epreds)
                   (subst-compose
                    (subst-compose (subst-compose csubst tsubst) esubst)
                    (unify (form-type tform) (lookup "Bool" :type))))))))
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
        (bind env :value name var)
        (list self var (resolve value env))))
    ((setf (var-type name) (to-scheme (make-instance 'tyvar :kind 1)))
      (multiple-value-bind (vform vpreds vsubst)
          (infer-expr value)
        (let* ((final-subst (subst-compose
                             vsubst
                             (unify (head (fresh-instance (var-type name)))
                                    (form-type vform))))
               (vty (subst-apply final-subst (form-type vform))))
          ;; TODO: Propogate quantification substitution to form body
          (setf (var-type name) (subst-apply final-subst (var-type name)))
          (values (quantify-form (free-vars vty)
                                 (subst-apply final-subst vpreds)
                                 (make-form vty (list self name (subst-code final-subst vform))))
                  nil))))
    (module builder type
      ;; TODO: Non-function values
      ;; TODO: Auto-create pointer specialization
      (unless (vars type)
        (let ((llvm-value (codegen module builder value)))
          (setf (llvm:value-name llvm-value) (var-fqn name)
                (llvm:linkage llvm-value) :internal
                (llvm name) llvm-value)))))

(defspecial "module" self (name imports &rest exports)
    (env
      (assert (toplevel? env) () "Cannot change modules below the top level")
      (let ((import-modules (mapcar (rcurry #'lookup :value env) imports)))
        (let ((module (make-module name (module env)
                                   import-modules
                                   exports)))
          (values (list* self name import-modules exports)
                  module))))
    ((declare (ignore name imports exports))
      nil)
    (lmodule builder type
      (declare (ignore builder name exports type))
      (dolist (import imports)
        (dolist (export (exports import))
          ;; TODO: Import non-values
          (let ((remote-binding (lookup export :value (env import))))
            (when (and (typep remote-binding 'var)
                       (null (vars (var-type remote-binding))))
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
    (env (list self (type-eval type env) (resolve value env)))
    ((multiple-value-bind (form preds subst) (infer-expr value)
       (values form preds
               (subst-compose subst (unify (form-type form) type)))))
    (m b ty
      (declare (ignore m b ty type value))
      (error "What's this doing here?")))

(defspecial "interface" self (name vars supers &rest bindings)
    (env
      (let* ((subenv (make-subenv env))
             (gens (loop :for var :in vars
                         :for n :from 0
                         :for gen := (make-instance 'tygen
                                                    :number n)
                         :do (bind subenv :type var gen)
                         :collect gen))
             (preds (mapcar (rcurry #'constraint-eval subenv) supers))
             (interface (make-instance 'interface
                                       :name name
                                       :vars gens
                                       :supers preds)))
        (bind env :interface name interface)
        (loop :for (name type default) :in bindings
              :for resolved := (type-resolve type subenv)
              :do (loop :for (gen . kind) :in (infer-kinds resolved)
                        :do (setf (kind gen) kind))
                  (bind env :value name
                        (make-instance
                         'var
                         :name name
                         :env env
                         :var-type
                         (make-instance 'scheme
                                        :vars gens
                                        :inner-type
                                        (qualify (list (make-pred interface gens))
                                                 (type-construct resolved subenv))))))
        nil))
    ((declare (ignore name vars supers bindings)))
    (m b ty
      (declare (ignore name vars supers bindings m b ty))))

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
        (bind env :value name var)
        (list* self var rtype typed-args)))
    ((declare (ignore name return-type args))
      nil)
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
