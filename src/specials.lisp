(in-package #:bitlisp)

(defmacro def-type-special (name self (&rest args) &body stages)
  ":resolve :construct"
  (with-gensyms (sym)
    `(let* ((,sym (make-bl-symbol ,name))
            (,self (make-special-tycon :name ,sym)))
       (bind *primitives-env* :type ,sym ,self)
       (setf
        ,@(loop :for stage :in stages
                :nconc
                (destructuring-ecase stage
                  ((:resolve (env) &body body)
                   `((special-tycon-resolver ,self) (lambda (,env ,@args) ,@body)))
                  ((:construct &body body)
                   `((special-tycon-constructor ,self) (lambda ,args ,@body)))))))))

(def-type-special "forall" self (vars constraints type)
  (:resolve
   (env)
   (let ((subenv (make-subenv env)))
     (let ((tyvars (loop :repeat (length vars)
                         :collect (make-instance 'tyvar))))
       (mapc (curry #'bind subenv :type) (mapcar #'ensure-bl-sym vars) tyvars)
       (list self tyvars
             (mapcar (rcurry #'constraint-eval subenv)
                     constraints)
             (infer-kinds (type-resolve type subenv))))))
  (:construct
   (declare (ignore vars))
   (values (type-construct type) constraints)))

(def-type-special "type-of" self (value)
  (:resolve (env) (list self (resolve value env)))
  (:construct (form-type (infer-expr value))))

(defmacro defspecial (name self (&rest args) &body stages)
  `(let ((,self (make-special-op :name ,name)))
     (setf
      ,@(loop :for stage :in stages
              :nconc
              (destructuring-ecase stage
                ((:resolve (env) &body body)
                 `((special-op-resolver ,self) (lambda (,env ,@args) ,@body)))
                ((:infer &body body)
                 `((special-op-inferrer ,self) (lambda (,@args) ,@body)))
                ((:codegen (module builder complete-type) &body body)
                 `((special-op-codegen ,self)
                   (lambda (,module ,builder ,complete-type ,@args) ,@body))))))
     (bind *primitives-env* :value (make-bl-symbol ,name) ,self)))

(defspecial "lambda" self (args &rest body)
  (:resolve
   (env)
   (let* ((new-env (make-instance 'environment
                                  :parents (list env)
                                  :toplevel? nil
                                  :module (module env)))
          (arg-vars (mapcar (lambda (sym)
                              (make-instance 'value :name sym :env new-env
                                                    :value-type (to-scheme (make-instance 'tyvar :kind 1))))
                            args)))
     (mapc (curry #'bind new-env :value) args arg-vars)
     (list* self arg-vars
            (mapcar (rcurry #'resolve new-env) body))))
  (:infer
   (multiple-value-bind (forms preds subst) (infer-expr-seq body)
     (values (make-form (make-ftype (apply #'make-prodty (mapcar (compose #'head #'fresh-instance #'value-type) args))
                                    (form-type (lastcar forms)))
                        (list* self args forms))
             preds
             subst)))
  (:codegen
   (module builder type)
   (declare (ignore builder))
   (with-func (func local-builder module type
                    :arg-names (mapcar (compose #'name #'name) args))
     (mapc #'(setf llvm) (llvm:params func) args)
     (loop :for (form . rest) :on body
           :for genned := (codegen module local-builder form)
           :unless rest :do (llvm:build-ret local-builder genned)))))

(defspecial "if" self (condition then else)
  (:resolve
   (env)
   (list self (resolve condition env)
         (resolve then env) (resolve else env)))
  (:infer
   (multiple-value-bind (cform cpreds csubst) (infer-expr condition)
     (multiple-value-bind (tform tpreds tsubst) (infer-expr then)
       (multiple-value-bind (eform epreds esubst) (infer-expr else)
         (values (make-form (form-type tform) (list self cform tform eform))
                 (nconc cpreds tpreds epreds)
                 (subst-compose
                  (subst-compose (subst-compose csubst tsubst) esubst)
                  (unify (form-type tform) (lookup "bool" :type))))))))
  (:codegen
   (module builder type)
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
  (:resolve
   (env)
   (let ((var (make-instance 'value
                             :name name :env env
                             :value-type (to-scheme (make-instance 'tyvar :kind 1)))))
     (bind env :value name var)
     (list self var (resolve value env))))
  (:infer
   (multiple-value-bind (vform vpreds vsubst) (infer-expr value)
     (let* ((final-subst (subst-compose vsubst
                                        (unify (head (fresh-instance (value-type name)))
                                               (form-type vform))))
            (vty (subst-apply final-subst (form-type vform))))
       ;; TODO: Pass in free type vars collected from non-global value bindings
       (multiple-value-bind (deferred retained) (split-preds nil (free-vars vty)
                                                             (subst-apply final-subst vpreds))
         (let ((final-form (quantify-form (free-vars vty)
                                          retained
                                          (make-form vty (subst-code final-subst vform)))))
           (setf (code name) (form-code final-form))
           (setf (value-type name) (form-type final-form))
           (values (make-form (form-type final-form) (list self name final-form))
                   deferred
                   nil))))))
  (:codegen
   (module builder type)
   ;; TODO: Non-function values
   ;; TODO: Auto-create pointer specialization of polymorphic functions
   (unless (vars type)
     (let ((llvm-value (codegen module builder value)))
       (setf (llvm:value-name llvm-value) (var-fqn name)
             (llvm:linkage llvm-value) :internal
             (llvm name) llvm-value)))))

(defspecial "def-as" self (declared-type name value)
  (:resolve
   (env)
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
  (:infer
   (multiple-value-bind (form preds subst) (infer-expr value)
     (let* ((unif-type (fresh-instance declared-type))
            (final-subst (subst-compose subst (unify (head unif-type)
                                                     (form-type form))))
            (declared-preds (subst-apply final-subst (context unif-type)))
            (final-type (subst-apply final-subst unif-type))
            (inferred-preds (remove-if (curry #'entail declared-preds)
                                       (subst-apply final-subst preds))))
       (multiple-value-bind (deferred retained) (split-preds nil (free-vars final-type)
                                                             inferred-preds)
         (let ((final-form (quantify-form (free-vars final-type)
                                          declared-preds
                                          (make-form (head final-type)
                                                     (subst-code final-subst value)))))
           (setf (code name) (form-code final-form))
           (cond
             (retained
              (error "Declared context ~A is weaker than ~A"
                     declared-preds inferred-preds))
             ((not (bl-type= (value-type name) (form-type final-form)))
              (error "Declared type ~A is more general than inferred type ~A"
                     (value-type name) (form-type final-form))))
           (values (make-form (form-type final-form) (list self (form-type final-form)
                                                           name final-form))
                   deferred
                   nil))))))
  (:codegen
   (module builder type)
   (declare (ignore declared-type))
   ;; TODO: Non-function values
   ;; TODO: Auto-create pointer specialization of polymorphic functions
   (unless (vars type)
     (let ((llvm-value (codegen module builder value)))
       (setf (llvm:value-name llvm-value) (var-fqn name)
             (llvm:linkage llvm-value) :internal
             (llvm name) llvm-value)))))

(defspecial "def-type-name" self (name type)
  (:resolve
   (env)
   (let ((ty (type-eval type env)))
     (when (free-vars ty)
       (error "Cannot declare another name for a polytype (~A)" ty))
     (bind env :type name ty)
     nil))
  (:infer (declare (ignore name type)))
  (:codegen (m b c) (declare (ignore name type m b c))))

(defspecial "module" self (name imports &rest exports)
  (:resolve
   (env)
   (assert (toplevel? env) () "Cannot change modules below the top level")
   (let ((import-modules (mapcar (rcurry #'lookup :value env) imports))
         value-names type-names)
     (dolist (export exports)
       (if (atom export)
           (push export value-names)
           (cond
             ((eq (sym "type") (first export))
              (setf type-names (rest export)))
             (t (error "Unrecognized export type ~A" (first export))))))
     (let ((module (make-module name (module env) import-modules
                                :value-exports value-names
                                :type-exports type-names)))
       (values (list* self (lookup name :value env) import-modules exports)
               module))))
  (:infer (make-form (lookup "unit") (list* self name imports exports)))
  (:codegen
   (lmodule builder type)
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
  (:resolve (env) (list self (type-resolve type env) (resolve value env)))
  (:infer
   (multiple-value-bind (form val-preds subst) (infer-expr value)
     (multiple-value-bind (ty ty-preds) (type-construct type)
       (values form (nconc ty-preds val-preds)
               (subst-compose subst (unify (form-type form) ty))))))
  (:codegen
   (m b ty)
   (declare (ignore m b ty type value))
   (error "What's this doing here?")))

(defspecial "interface" self (name vars supers &rest bindings)
  (:resolve
   (env)
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
     (bind env :type name interface)
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
                                              (type-construct resolved))))))
     nil))
  (:infer (declare (ignore name vars supers bindings)))
  (:codegen (m b ty)
            (declare (ignore name vars supers bindings m b ty))))

;; (defspecial "implement" self (vars constraints interface &rest values)
;;   (:resolve
;;    (env)
;;    (let* ((subenv (make-subenv env))
;;           (args (mapcar (compose #'infer-kinds (rcurry #'type-resolve-free subenv)) types)))
;;      (list* self interface types constraints values)))
;;   (:infer (make-form (lookup "Unit") (list* self interface type constraints)))
;;   (:codegen (module builder ctype) (error "TODO")))

(defspecial "cdef" self (name return-type &rest args)
  (:resolve
   (env)
   (let* ((rtype (type-eval return-type env))
          (typed-args (mapcar (lambda (arg)
                                (list (first arg) (type-eval (second arg) env)))
                              args))
          (var (make-instance 'value
                              :name name
                              :env env
                              :value-type (to-scheme
                                           (if args
                                               (apply #'make-ftype
                                                      (apply #'make-prodty
                                                             (mapcar #'second
                                                                     typed-args))
                                                      rtype)
                                               (tyapply (lookup "ptr") return-type))))))
     (bind env :value name var)
     (list* self var rtype typed-args)))
  (:infer
   (make-form (value-type name)
              (list* self name return-type args)))
  (:codegen
   (module builder ctype)
   (declare (ignore builder return-type))
   (if args
       ;; Function
       (let ((cfunc (llvm:add-function module (name (name name))
                                       (llvm ctype))))
         (with-func (blfunc inner-builder module ctype
                            :attributes '(:inline-hint))
           (setf (llvm name) blfunc)
           (llvm:build-ret inner-builder
                           (llvm:build-call inner-builder
                                            cfunc (llvm:params blfunc) ""))))
       ;; Variable
       (llvm:add-global module (llvm ctype) (name (name name))))))

(defspecial "size-of" self (monotype)
  (:resolve (env) (list self (type-eval monotype env)))
  (:infer (make-form (type-eval '("int" 64)) (list self monotype)))
  (:codegen
   (module builder type)
   (declare (ignore module builder type))
   (llvm:size-of (llvm monotype))))

(defspecial "def-type" self (name args type)
  (:resolve
   (env)
   (let* ((subenv (make-subenv env))
          (tyvars (loop :for arg :in args
                        :for tyvar := (make-instance 'tyvar)
                        :do (bind subenv :type arg tyvar)
                        :collect tyvar)))
     (bind env :type name
           (make-instance 'tycon
                          :name name
                          :kind (length tyvars)
                          :llvm (llvm (type-eval type subenv)))))
   nil)
  (:infer (declare (ignore name args type)))
  (:codegen (m b ty) (declare (ignore m b ty name args type))))
