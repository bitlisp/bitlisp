(in-package #:bitlisp)

(defgeneric free-vars (type)
  (:method ((type t))
    (if (typevar? type)
        (list type)
        nil)))
(defgeneric bl-type= (a b)
  (:method ((a t) (b t))
    (eql a b)))
(defgeneric subst-apply (substitution thing)
  (:method (s (thing t))
    (loop :for (var . value) :in s
          :do (when (bl-type= var thing)
                (setf thing value)))
    thing))

(defun concrete-type (type)
  (null (free-vars type)))

(defun typevar? (x)
  (integerp x))

(defclass bl-type () ())

(defclass unit-type (bl-type) ())

(defmethod print-object ((type unit-type) stream)
  (print-unreadable-object (type stream :type t)))

(defmethod bl-type= ((a unit-type) (b unit-type))
  t)

(defclass simple-type (bl-type)
  ((llvm :initarg :llvm :reader llvm)))

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

(defclass constructed-type (bl-type)
  ((constructor :initarg :constructor :reader constructor)
   (args :initarg :args :reader args)))

(defmethod free-vars ((type constructed-type))
  (remove-duplicates
   (nconc (free-vars (constructor type))
          (reduce 'nconc (args type)
                  :key 'free-vars))))

(defmethod bl-type= ((a constructed-type) (b constructed-type))
  (and (bl-type= (constructor a) (constructor b))
       (every #'bl-type= (args a) (args b))))

(defmethod print-object ((type constructed-type) stream)
  (print-unreadable-object (type stream)
    (format stream "~A~{ ~A~}" (constructor type) (args type))))

(defmethod subst-apply (substitution (type constructed-type))
  (make-instance 'constructed-type
                 :constructor (subst-apply substitution (constructor type))
                 :args (mapcar (curry 'subst-apply substitution) (args type))))

(defun make-ftype (return-type &rest arg-types)
  (make-instance 'constructed-type
                 ;; TODO: Real function constructor
                 :constructor :func
                 :args (list* return-type arg-types)))

(defclass quantified-type (bl-type)
  ((variables :initarg :variables :reader variables)
   (constraints :initarg :constraints :initform nil :reader constraints)
   (inner-type :initarg :inner-type :reader inner-type)))

(defmethod print-object ((type quantified-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "(forall (~{~A~^ ~}) (~{~A~^ ~}) ~A)"
            (variables type) (constraints type) (inner-type type))))

(defmethod free-vars ((type quantified-type))
  (remove-if (rcurry #'member (variables type))
             (free-vars (inner-type type))))

(defmethod bl-type= ((a quantified-type) (b quantified-type))
  (and (= (length (variables a)) (length (variables b)))
       (let ((subst (reduce 'subst-compose (mapcar 'make-subst
                                                   (variables b) (variables a)))))
         (every #'constraint= (constraints a) (mapcar #'subst-apply (constraints b)))
         (bl-type= (inner-type a) (subst-apply subst (inner-type b))))))

(defmethod subst-apply (substitution (type quantified-type))
  (make-instance 'quantified-type
                 :variables (mapcar (curry 'subst-apply substitution) (variables type))
                 :constraints (mapcar (curry 'subst-apply substitution) (constraints type))
                 :inner-type (subst-apply substitution (inner-type type))))
