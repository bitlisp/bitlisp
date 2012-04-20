(in-package #:bitlisp)

(defmacro def-type-special (name self (&rest args) (env &rest resolver) constructor)
  (with-gensyms (sym)
    `(let* ((,sym (make-bl-symbol ,name))
            (,self (make-special-tycon :name ,sym)))
       (bind *primitives-env* :type ,sym ,self)
       (setf (special-tycon-resolver ,self) (lambda (,env ,@args) ,@resolver)
             (special-tycon-constructor ,self) (lambda ,args ,@constructor)))))

(def-type-special "forall" self (vars constraints type)
    (env (let ((subenv (make-subenv env)))
           (let ((tyvars (loop :repeat (length vars)
                               :collect (make-instance 'tyvar))))
             (mapc (curry #'bind subenv :type) vars tyvars)
             (list self tyvars
                   (mapcar (rcurry #'constraint-eval subenv)
                           constraints)
                   (infer-kinds (type-resolve type subenv))))))
    ((declare (ignore vars))
     (values (type-construct type) constraints)))

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
                                 (make-instance 'value :name sym :env new-env))
                               args)))
        (mapc (curry #'bind new-env :value) args arg-vars)
        (list* self arg-vars
               (mapcar (rcurry #'resolve new-env) body))))
    ((let ((arg-types (loop :repeat (length args) :collect (make-instance 'tyvar :kind 1))))
       (mapc (lambda (arg ty) (setf (value-type arg) (to-scheme ty)))
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
      (let ((var (make-instance 'value
                                :name name :env env
                                :value-type (to-scheme (make-instance 'tyvar :kind 1)))))
        (bind env :value name var)
        (list self var (resolve value env))))
    ((multiple-value-bind (vform vpreds vsubst) (infer-expr value)
       (let* ((final-subst (subst-compose vsubst
                                          (unify (head (fresh-instance (value-type name)))
                                                 (form-type vform))))
              (vty (subst-apply final-subst (form-type vform))))
         (setf (value-type name) (subst-apply final-subst (value-type name)))
         ;; TODO: Pass in free type vars collected from non-global value bindings
         (multiple-value-bind (deferred retained) (split-preds nil (free-vars vty)
                                                               (subst-apply final-subst vpreds))
           (let ((final-form (quantify-form (free-vars vty)
                                            retained
                                            (make-form vty (subst-code final-subst vform)))))
             (setf (form name) final-form)
             (values (make-form (form-type final-form) (list self name final-form))
                     deferred
                     nil))))))
    (module builder type
      ;; TODO: Non-function values
      ;; TODO: Auto-create pointer specialization of polymorphic functions
      (unless (vars type)
        (let ((llvm-value (codegen module builder value)))
          (setf (llvm:value-name llvm-value) (var-fqn name)
                (llvm:linkage llvm-value) :internal
                (llvm name) llvm-value)))))

(defspecial "def-as" self (declared-type name value)
    (env
      (let* ((subenv (make-subenv env))
             (scheme
               (multiple-value-bind (ty preds) (type-eval declared-type env)
                 (quantify (free-vars ty) preds ty)))
             (var (make-instance 'value
                                 :name name :env env
                                 :value-type scheme)))
        (bind env :value name var)
        ;; Resolve in type-binding subenv for scoped type variables!
        (list self scheme var (resolve value subenv))))
    ((multiple-value-bind (form preds subst) (infer-expr value)
       (let* ((unif-type (fresh-instance declared-type))
              (final-subst (subst-compose subst (unify (head unif-type)
                                                       (form-type form))))
              (final-type (subst-apply final-subst unif-type))
              (final-preds (nconc (context final-type) (subst-apply final-subst preds))))
         (multiple-value-bind (deferred retained) (split-preds nil (free-vars final-type)
                                                               final-preds)
           (let ((final-form (quantify-form (free-vars final-type)
                                            retained
                                            (make-form (head final-type)
                                                       (subst-code final-subst value)))))
             (setf (form name) final-form)
             (values (make-form (form-type final-form) (list self (form-type final-form)
                                                             name final-form))
                     deferred
                     nil))))))
    (module builder type
      (declare (ignore declared-type))
      ;; TODO: Non-function values
      ;; TODO: Auto-create pointer specialization of polymorphic functions
      (unless (vars type)
        (let ((llvm-value (codegen module builder value)))
          (setf (llvm:value-name llvm-value) (var-fqn name)
                (llvm:linkage llvm-value) :internal
                (llvm name) llvm-value)))))

(defspecial "def-type-name" self (name type)
    (env
      (let ((ty (type-eval type env)))
        (bind env :type name ty)
        nil))
    ((declare (ignore name type)))
    (m b c
      (declare (ignore name type m b c))))

(defspecial "module" self (name imports &rest exports)
    (env
      (assert (toplevel? env) () "Cannot change modules below the top level")
      (let ((import-modules (mapcar (rcurry #'lookup :value env) imports))
            value-names type-names interface-names)
        (dolist (export exports)
          (if (atom export)
              (push export value-names)
              (ecase (first export)
                (type (setf type-names (rest export)))
                (interface (setf interface-names (rest export))))))
        (let ((module (make-module name (module env) import-modules
                                   :value-exports value-names
                                   :type-exports type-names
                                   :interface-exports interface-names)))
          (values (list* self (lookup name :value env) import-modules exports)
                  module))))
    ((declare (ignore name imports exports))
      (make-form (lookup "unit") (list* self name imports exports)))
    (lmodule builder type
      (declare (ignore builder name exports type))
      (dolist (import imports)
        (dolist (export (value-exports import))
          (let ((remote-binding (lookup export :value (env import))))
            (when (and (typep remote-binding 'value)
                       (null (vars (value-type remote-binding))))
              (let ((val (if (ftype? (value-type remote-binding))
                             (prog1-let (func (llvm:add-function
                                               lmodule
                                               (symbol-fqn import export)
                                               (llvm (value-type remote-binding))))
                               (setf (llvm:function-calling-convention func) :fast))
                             (llvm:add-global lmodule
                                              (llvm (value-type remote-binding))
                                              (symbol-fqn import export)))))
                (setf (llvm:linkage val) :external
                      (llvm remote-binding) val))))))))

(defspecial "the" self (type value)
    (env (list self (type-resolve type env) (resolve value env)))
    ((multiple-value-bind (form val-preds subst) (infer-expr value)
       (multiple-value-bind (ty ty-preds) (type-construct type)
         (values form (nconc ty-preds val-preds)
                 (subst-compose subst (unify (form-type form) ty))))))
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
              :for resolved := (infer-kinds (type-resolve type subenv))
              :do (bind env :value name
                        (make-instance
                         'value
                         :name name
                         :env env
                         :value-type
                         (make-instance 'scheme
                                        :vars gens
                                        :inner-type
                                        (qualify (list (apply #'make-pred interface gens))
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
             (var (make-instance 'value
                                 :name name
                                 :env env
                                 :value-type (if args
                                                 (apply #'make-ftype
                                                        (apply #'make-prodty
                                                               (mapcar #'second
                                                                       typed-args))
                                                         rtype)
                                                 (tyapply (lookup "ptr") return-type)))))
        (bind env :value name var)
        (list* self var rtype typed-args)))
    ((make-form (value-type name)
                (list* self name return-type args)))
    (module builder ctype
      (declare (ignore builder return-type))
      (if args
          ;; Function
          (let* ((cfunc (llvm:add-function module (name (name name)) (llvm ctype)))
                 (blfunc (llvm:add-function module (var-fqn name) (llvm ctype)))
                 (entry (llvm:append-basic-block blfunc "entry")))
            (llvm:add-function-attributes blfunc :inline-hint)
            (setf (llvm:function-calling-convention blfunc) :fast
                  (llvm name) blfunc)
            (llvm:with-object (builder builder)
              (llvm:position-builder-at-end builder entry)
              (llvm:build-ret builder (llvm:build-call builder cfunc (llvm:params blfunc) ""))))
          ;; Variable
          (llvm:add-global module (llvm ctype) (name (name name))))))
