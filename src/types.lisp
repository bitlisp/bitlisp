(in-package #:bitlisp)

(defgeneric free-vars (type)
  (:method (type)
    (if (typevar? type)
        (list type)
        nil)))
(defgeneric bl-type= (a b)
  (:method (a b)
    "Base case to catch incompatible types"
    (declare (ignore a b))
    nil)
  (:method ((a integer) (b integer))
    "Integral types intended for use as primitive constructor parameters"
    (= a b)))
(defgeneric subst-apply (substitution thing)
  (:method (s thing)
    "Default implementation for simple, flat substitutions"
    (loop :for (var . value) :in s
          :do (when (bl-type= var thing)
                (setf thing value)))
    thing))

(defun type-eval (code &optional (env *primitives-env*))
  (etypecase code
    (integer code)
    ((or bl-symbol string) (lookup code env))
    (list (destructuring-bind (constructor &rest args) code
            (make-instance 'constructed-type
                           :constructor (type-eval constructor env)
                           :args (mapcar (rcurry #'type-eval env) args))))))

(defun subst-constraints (substitutions constraints)
  (mapcar (lambda (constraint)
            (destructuring-bind (l . r) constraint
              (cons (subst-apply substitutions l)
                    (subst-apply substitutions r))))
          constraints))

(defun constraint= (a b)
  "Check that two equality constraints are interchangable."
  (check-type a cons)
  (check-type b cons)
  (or (and (bl-type= (car a) (car b))
           (bl-type= (cdr a) (cdr b)))
      (and (bl-type= (car a) (cdr b))
           (bl-type= (cdr a) (car b)))))

(defun complete-type (type)
  (null (free-vars type)))

(defclass quant-var ()
  ((name :initarg :name :reader name)))

(defmethod print-object ((type quant-var) stream)
  (print-unreadable-object (type stream :type t)
    (princ (name type) stream)))

(defmethod bl-type= ((l quant-var) (r quant-var))
  (eq l r))

(defclass type-var ()
  ((number :initarg :number :reader number)))

(defmethod bl-type= ((l type-var) (r type-var))
  (= (number l) (number r)))

(defmethod print-object ((type type-var) stream)
  (print-unreadable-object (type stream)
    (format stream "t~D" (number type))))

(defun typevar? (x)
  (typep x 'type-var))

(defclass bl-type () ())

(defclass simple-type (bl-type)
  ((name :initarg :name :reader name)
   (llvm :initarg :llvm :reader llvm)))

(defmethod print-object ((type simple-type) stream)
  (print-unreadable-object (type stream)
    (princ (name type) stream)))

(defclass machine-int (bl-type)
  ((bits :initarg :bits :reader bits)
   (signed :initarg :signed :reader signed)))

(defmethod llvm ((type machine-int))
  (llvm:int-type (bits type)))

(defmethod print-object ((type machine-int) stream)
  (print-unreadable-object (type stream)
    (format stream "~:[u~;~]int~D" (signed type) (bits type))))

(defmethod bl-type= ((a machine-int) (b machine-int))
  (and (= (bits a) (bits b))
       (eq (signed a) (signed b))))

(defclass type-constructor ()
  ((name :initarg :name :reader name)
   (llvm :initarg :llvm :reader llvm)))

(defmethod print-object ((ctor type-constructor) stream)
  (print-unreadable-object (ctor stream :type t)
    (princ (name ctor) stream)))

(defmethod bl-type= ((l type-constructor) (r type-constructor))
  (eq l r))

(defclass constructed-type (bl-type)
  ((constructor :initarg :constructor :reader constructor)
   (args :initarg :args :reader args)))

(defmethod free-vars ((type constructed-type))
  (remove-duplicates
   (nconc (free-vars (constructor type))
          (reduce 'nconc (args type)
                  :key 'free-vars))))

(defmethod bl-type= ((a constructed-type) (b constructed-type))
  (and (eq (constructor a) (constructor b))
       (every #'bl-type= (args a) (args b))))

(defmethod print-object ((type constructed-type) stream)
  (print-unreadable-object (type stream)
    (format stream "~A~{ ~A~}" (name (constructor type)) (args type))))

(defmethod subst-apply (substitution (type constructed-type))
  (make-instance 'constructed-type
                 :constructor (subst-apply substitution (constructor type))
                 :args (mapcar (curry 'subst-apply substitution) (args type))))

(defmethod llvm ((type constructed-type))
  (apply (llvm (constructor type)) (args type)))

(defclass universal-type (bl-type)
  ((variables :initarg :variables :reader variables)
   (constraints :initarg :constraints :initform nil :reader constraints)
   (inner-type :initarg :inner-type :reader inner-type)))

(defmethod print-object ((type universal-type) stream)
  (print-unreadable-object (type stream)
    (format stream "∀ (~{~A~^ ~}) (~{~A~^ ~}) ~A"
            (variables type) (constraints type) (inner-type type))))

(defmethod free-vars ((type universal-type))
  (remove-if (rcurry #'member (variables type))
             (free-vars (inner-type type))))

(defmethod bl-type= ((a universal-type) (b universal-type))
  (and (= (length (variables a)) (length (variables b)))
       (let ((subst (reduce 'subst-compose (mapcar 'make-subst
                                                   (variables b) (variables a)))))
         (every #'constraint= (constraints a) (subst-constraints subst (constraints b)))
         (bl-type= (inner-type a) (subst-apply subst (inner-type b))))))

(defun instantiate-type (universal-type vargen)
  (check-type universal-type universal-type)
  (let ((subst (mapcar (lambda (x) (cons x (funcall vargen)))
                       (variables universal-type))))
    (values (subst-apply subst
                         (inner-type universal-type))
            (subst-constraints subst (constraints universal-type)))))

(defmethod subst-apply (substitution (type universal-type))
  (declare (ignore substitution))
  (error "Tried to apply a substitution to ~A" type))

(defun concretify-type (universal-type &rest types)
  ;; TODO: Check that the concrete types fulfill the constraints
  (let ((subst (reduce 'subst-compose
                       (mapcar 'make-subst
                               (variables universal-type) types))))
    (subst-apply subst (inner-type universal-type))))

(defclass product-type (bl-type)
  ((args :initarg :args :reader args)))

(defmethod print-object ((type product-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~A~^ ~}" (args type))))

(defmethod free-vars ((type product-type))
  (remove-duplicates (reduce 'nconc (args type)
                             :key 'free-vars)))

(defmethod bl-type= ((a product-type) (b product-type))
  (and (= (length (args a)) (length (args b)))
       (every #'bl-type= (args a) (args b))))

(defmethod subst-apply (substitution (type product-type))
  (make-instance 'product-type
                 :args (mapcar (curry 'subst-apply substitution) (args type))))
