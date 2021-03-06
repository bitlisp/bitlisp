(in-package #:bitlisp)

(defgeneric free-vars (type)
  (:method (type)
    (declare (ignore type))
    nil)
  (:method ((types list))
    (delete-duplicates (mapcan #'free-vars types))))
(defgeneric bl-type= (a b)
  (:method (a b)
    "Base case to catch incompatible types"
    (declare (ignore a b))
    nil)
  (:method ((a integer) (b integer))
    "Integral types intended for use as primitive constructor parameters"
    (= a b)))
(defgeneric subst-apply (substitution type)
  (:method (s ty)
    (declare (ignore s))
    ty)
  (:method (s (list list))
    (mapcar (curry #'subst-apply s) list)))

;;; Substitutions are alists of the form ((var1 . value1) (var2 . value2))
(defun make-subst (var value)
  "Create a substitution consisting of a single replacement of the form var := value"
  (list (cons var value)))

(defun subst-compose (sigma gamma)
  "Compose two substitutions (see TAPL 22.1.1)"
  (nconc (loop :for (var . value) :in gamma
               :nconc (make-subst var (subst-apply sigma value)))
         (remove-if (lambda (x) (find x gamma :key #'car))
                    sigma)))

(defun subst-constraints (substitutions constraints)
  (mapcar (lambda (constraint)
            (destructuring-bind (l . r) constraint
              (cons (subst-apply substitutions l)
                    (subst-apply substitutions r))))
          constraints))

(defgeneric kind (type)
  (:documentation "An integer ≥1 indicating type constructor arity."))

(defclass bl-type () ())

(defclass tyvar (bl-type)
  ((kind :initarg :kind :accessor kind)))

(defmethod subst-apply (s (var tyvar))
  (or (assoc-value s var :test 'eq) var))

(defmethod bl-type= ((a tyvar) (b tyvar))
  (eq a b))

(defmethod free-vars ((var tyvar))
  (list var))

(defclass tygen (bl-type)
  ((number :initarg :number :reader number)
   (kind :initarg :kind :accessor kind)))

(defmethod print-object ((ty tygen) stream)
  (print-unreadable-object (ty stream :type t)
    (princ (number ty) stream)))

(defmethod bl-type= ((a tygen) (b tygen))
  (and (= (number a) (number b))
       (= (kind a) (kind b))))

(defclass tycon (bl-type)
  ((name :initarg :name :reader name)
   (kind :initarg :kind :reader kind)
   (llvm :initarg :llvm :reader llvm)))

(defmethod print-object ((ty tycon) stream)
  (print-unreadable-object (ty stream :type t)
    (princ (name ty) stream)))

(defmethod bl-type= ((a tycon) (b tycon))
  (eq a b))

(defclass tyapp (bl-type)
  ((operator :initarg :operator :reader operator)
   (args :initarg :args :reader args)))

(defmethod print-object ((ty tyapp) stream)
  (print-unreadable-object (ty stream :type t)
    (format stream "~A~{ ~S~}" (name (operator ty)) (args ty))))

(defmethod kind ((type tyapp))
  (- (kind (operator type)) (length (args type))))

(defmethod subst-apply (s (app tyapp))
  (make-instance 'tyapp
                 :operator (subst-apply s (operator app))
                 :args (mapcar (curry #'subst-apply s) (args app))))

(defmethod bl-type= ((a tyapp) (b tyapp))
  (and (bl-type= (operator a) (operator b))
       (every #'bl-type= (args a) (args b))))

(defmethod free-vars ((type tyapp))
  (delete-duplicates (nconc (free-vars (operator type))
                            (free-vars (args type)))))

(defmethod llvm ((ty tyapp))
  (assert (= 1 (kind ty)) ()
          "~A has kind ~A ≠ 1 and therefore cannot exist at runtime!"
          ty)
  (apply (llvm (operator ty)) (args ty)))

(defclass tyqual (bl-type)
  ((context :initarg :context :reader context
            :documentation "List of predicates")
   (head :initarg :head :reader head)))

(defmethod print-object ((ty tyqual) stream)
  (print-unreadable-object (ty stream :type t)
    (format stream "(~{~A~^ ~}) ~A" (context ty) (head ty))))

(defmethod subst-apply (s (qual tyqual))
  (make-instance 'tyqual
                 :context (subst-apply s (context qual))
                 :head (subst-apply s (head qual))))

(defmethod free-vars ((qual tyqual))
  (delete-duplicates (nconc (free-vars (context qual))
                            (free-vars (head qual)))))

(defmethod bl-type= ((a tyqual) (b tyqual))
  (and (every #'bl-type= (context a) (context b))
       (bl-type= (head a) (head b))))

(defmethod llvm ((ty tyqual))
  "Passes through to head"
  (llvm (head ty)))

(defun qualify (context head)
  (make-instance 'tyqual :context context :head head))

(defclass pred ()
  ((interface :initarg :interface :reader interface)
   (args :initarg :args :reader args)))

(defmethod print-object ((p pred) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A~{ ~A~}" (interface p) (args p))))

(defmethod subst-apply (s (pred pred))
  (make-instance 'pred
                 :interface (interface pred)
                 :args (subst-apply s (args pred))))

(defmethod free-vars ((pred pred))
  (free-vars (args pred)))

(defmethod bl-type= ((a pred) (b pred))
  (and (eq (interface a) (interface b))
       (every #'bl-type= (args a) (args b))))

(defun make-pred (interface &rest args)
  (make-instance 'pred
                 :interface interface
                 :args args))


(defun tyapply (operator &rest args)
  (assert (> (kind operator) (length args))
          (operator)
          "Cannot apply type of kind ~A to ~A arguments!"
          (kind operator) (length args))
  (typecase operator
    (tyapp (make-instance 'tyapp
                          :operator (operator operator)
                          :args (append (args operator) args)))
    (t (make-instance 'tyapp :operator operator :args args))))

(defun type-resolve (code &optional (env *primitives-env*))
  (etypecase code
    (integer code)
    ((or symbol bl-symbol string) (lookup code :type env))
    (list (let ((ctor (type-resolve (first code) env)))
            (typecase ctor
              (special-tycon (apply (special-tycon-resolver ctor)
                                    env (rest code)))
              (t (cons ctor (mapcar (rcurry #'type-resolve env) (rest code)))))))))

(defun type-construct (resolved)
  (etypecase resolved
    ((or integer bl-type) resolved)
    (list (unless (null resolved)
            (destructuring-bind (constructor &rest args) resolved
              (typecase constructor
                (special-tycon (apply (special-tycon-constructor constructor)
                                      args))
                (t (apply #'tyapply (mapcar #'type-construct resolved)))))))))

(defun type-eval (code &optional (env *primitives-env*))
  (type-construct (type-resolve code env)))

(defun constraint-eval (code &optional (env *primitives-env*))
  (destructuring-bind (interface &rest args) code
    (make-instance 'pred
                   :interface (lookup interface :type env)
                   :args (mapcar (rcurry #'type-eval env) args))))

(defun make-ftype (arg-type return-type)
  (tyapply (lookup "func" :type) arg-type return-type))

(defun make-prodty (&rest types)
  (case (length types)
    (0 (lookup "unit" :type))
    (1 (first types))
    (otherwise (make-instance 'tyapp
                              :operator (lookup "*" :type)
                              :args (list (first types)
                                          (apply #'make-prodty (rest types)))))))

(defclass interface ()
  ((name :initarg :name :reader name)
   (supers :initarg :supers :reader supers
           :documentation "List of predicates on signature vars")
   (vars :initarg :vars :reader vars
         :documentation "List of vars paramterized on")))

(defun implementations (interface env)
  (mapcar #'predicate (lookup-implementations interface env)))

(defmethod kind ((interface interface))
  (length (vars interface)))

(defmethod print-object ((i interface) stream)
  (print-unreadable-object (i stream :type t)
    (princ (name i) stream)))

(defun preds-overlap (a b)
  (handler-case (unify a b)
    (simple-error nil)))

(defun by-super (pred)
  (cons pred (mapcan (compose #'by-super (curry #'instantiate (args pred)))
                     (supers (interface pred)))))

(defun by-impl (pred)
  (some (lambda (qual)
          (let ((subst (handler-case (match (head qual) pred)
                         (simple-error nil))))
            (if subst
                (mapcar (curry #'subst-apply subst)
                        (context qual))
                nil)))
        (implementations (interface pred))))

(defun entail (givens pred)
  (or
   ;; pred is in givens or their collected superclasses
   (some (compose (rcurry (curry #'member pred) :test #'bl-type=)
                  #'by-super)
         givens)
   ;; pred can be implemented using other interfaces
   (let ((new-preds (by-impl pred)))
     (and new-preds (every (curry #'entail givens) new-preds)))))

(defun normal-form? (pred)
  (labels ((test (type)
             (etypecase type
               (tyvar t)
               (tycon nil)
               (tyapp (every #'test (args type))))))
    (every #'test (args pred))))

(defun to-normal-form (pred)
  (if (normal-form? pred)
      (list pred)
      (if-let (reduced (by-impl pred))
        (list-to-normal-form reduced)
        (error "context reduction"))))

(defun list-to-normal-form (preds)
  (mapcan #'to-normal-form preds))

(defun simplify (preds)
  (labels ((helper (results inputs)
             (if (null inputs) results
                 (destructuring-bind (p &rest ps) inputs
                   (if (entail (append results ps) p)
                       (helper results ps)
                       (helper (cons p results) ps))))))
    (helper nil preds)))

(defun sc-entail (givens pred)
  (some (compose (rcurry (curry #'member pred) :test #'bl-type=)
                 #'by-super)
        givens))

(defun reduce-context (preds)
  (simplify (list-to-normal-form preds)))

(defclass scheme ()
  ((vars :initarg :vars :reader vars
         :documentation "tygen instances")
   (inner-type :initarg :inner-type :reader inner-type
               :documentation "Qualified type which may contain tygens"))
  (:documentation "forall"))

(defmethod print-object ((scheme scheme) stream)
  (print-unreadable-object (scheme stream :type t)
    (format stream "(~{~A~^ ~}) ~A"
            (vars scheme) (inner-type scheme))))

(defmethod subst-apply (subst (scheme scheme))
  (make-instance 'scheme
                 :vars (subst-apply subst (vars scheme))
                 :inner-type (subst-apply subst (inner-type scheme))))

(defmethod free-vars ((scheme scheme))
  (free-vars (inner-type scheme)))

(defmethod bl-type= ((a scheme) (b scheme))
  (bl-type= (inner-type a) (inner-type b)))

(defun subst-code (substitution tree)
  (cond
    ((consp tree)
     (cons (subst-code substitution (car tree))
           (subst-code substitution (cdr tree))))
    ((typep tree 'bl-type)
     (subst-apply substitution tree))
    (t tree)))

(defun quantify (tyvars preds type)
  (let* ((vars (remove-if-not (rcurry #'member (free-vars type)
                                      :test #'bl-type=)
                              tyvars))
         (gens (loop :for var :in vars
                     :for i :from 0
                     :collect (make-instance 'tygen
                                             :number i
                                             :kind (kind var))))
         (subst (mapcar #'cons vars gens)))
    (values (make-instance 'scheme
                           :vars gens
                           :inner-type
                           (make-instance 'tyqual
                                          :context (subst-apply subst preds)
                                          :head (subst-apply subst type)))
            subst)))

(defun quantify-form (tyvars preds form)
  (multiple-value-bind (type subst) (quantify tyvars preds (form-type form))
   (make-form type
              (subst-code subst (form-code form)))))

(defun to-scheme (monotype)
  (make-instance 'scheme :vars nil :inner-type
                 (make-instance 'tyqual :context nil :head monotype)))

(defgeneric instantiate (types gentype)
  (:documentation "Instantiate a type containg tygens")
  (:method (types gentype)
    "Default case is passthrough"
    (declare (ignore types))
    gentype)
  (:method (types (gentype list))
    (mapcar (curry #'instantiate types) gentype)))

(defmethod instantiate (types (gentype tygen))
  (nth (number gentype) types))

(defmethod instantiate (types (gentype tyapp))
  (make-instance 'tyapp
                 :operator (instantiate types (operator gentype))
                 :args (instantiate types (args gentype))))

(defmethod instantiate (types (gentype tyqual))
  (make-instance 'tyqual
                 :context (instantiate types (context gentype))
                 :head (instantiate types (head gentype))))

(defmethod instantiate (types (gentype pred))
  (make-instance 'pred
                 :interface (interface gentype)
                 :args (instantiate types (args gentype))))

(defun fresh-instance (scheme)
  (let ((vars (mapcar (lambda (v)
                        (make-instance 'tyvar :kind (kind v)))
                      (vars scheme))))
    (instantiate vars (inner-type scheme))))

(defun instantiate-form (types form)
  (destructuring-bind (type . code) form
    (make-form (instantiate types type)
               (if (listp code)
                   (mapcar (curry #'instantiate-form types) code)
                   (instantiate-form types code)))))

(defun specialize-form (poly-form monotype)
  (let ((vars (mapcar (lambda (v)
                        (make-instance 'tyvar :kind (kind v)))
                      (vars (form-type poly-form)))))
    (let* ((var-form (instantiate-form vars poly-form))
           (subst (unify (form-type var-form) monotype)))
      (subst-code subst var-form))))

(defclass implementation ()
  ((interface :initarg :interface :reader interface)
   (predicate :initarg :predicate :reader predicate
              :documentation "Qualified predicate describing the context (if any) and interface params corresponding to the implementation")
   (bindings :initarg :bindings :reader bindings
             :documentation "Value bindings implemented, corresponding to those prototyped by the interface")))
