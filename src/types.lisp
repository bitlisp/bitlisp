(in-package #:bitlisp)

(defgeneric free-vars (type)
  (:method ((type t))
    nil))
(defgeneric bl-type= (a b)
  (:method ((a t) (b t))
    (eql a b)))
(defmethod subst-apply (substitution (type t))
  (loop :for (var . value) :in substitution
        :do (when (bl-type= var type)
              (setf type value)))
  type)

(defun concrete-type (type)
  (null (free-vars type)))

(defun typevar? (x)
  (integerp x))

(defclass bl-type () ())

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
  ((constructor :initarg :constructor :accessor constructor)
   (args :initarg :args :accessor args)))

(defmethod free-vars ((type constructed-type))
  (append (free-vars (constructor type))
	  (mapcar #'free-vars (args type))))

(defmethod bl-type= ((a constructed-type) (b constructed-type))
  (and (bl-type= (constructor a) (constructor b))
       (every #'bl-type= (args a) (args b))))

(defmethod print-object ((type constructed-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "(~A~{ ~A~})" (constructor type) (args type))))

(defmethod subst-apply (substitution (type constructed-type))
  (setf (constructor type) (subst-apply substitution (constructor type)))
  (map-into (args type) (curry 'subst-apply substitution) (args type))
  type)
