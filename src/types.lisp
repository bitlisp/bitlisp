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

(defun subst-constraints (substitutions constraints)
  (mapcar (lambda (constraint)
            (destructuring-bind (l . r) constraint
              (cons (subst-apply substitutions l)
                    (subst-apply substitutions r))))
          constraints))

(defun constraint= (a b)
  "Check that two equality constraints are equal."
  (check-type a cons)
  (check-type b cons)
  (or (and (bl-type= (car a) (car b))
           (bl-type= (cdr a) (cdr b)))
      (and (bl-type= (car a) (cdr b))
           (bl-type= (cdr a) (car b)))))

(defun complete-type (type)
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
                 ;; TODO: Real constructor
                 :constructor :func
                 :args (list* return-type arg-types)))

(defun ftype? (type)
  (and (typep type 'constructed-type)
       ;; TODO: etc
       (eq :func (constructor type))))

(defun make-ptr (target-type)
  (make-instance 'constructed-type
                 ;; TODO: Real constructor
                 :constructor :ptr
                 :args (list target-type)))

(defmethod llvm ((type constructed-type))
  ;; TODO: Invoke an implementation stored in the constructor.
  (ecase (constructor type)
    (:func (llvm:function-type (llvm (first (args type))) (mapcar #'llvm (rest (args type)))))
    (:ptr (llvm:pointer-type (llvm (first (args type)))))))

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
