(in-package #:bitlisp)

(defgeneric free-vars (type))
(defgeneric conrete-type (type))
(defgeneric bl-type= (a b))

(defmethod free-vars ((type t))
  nil)
(defmethod bl-type= ((a t) (b t))
  (eql a b))

(defun concrete-type (type)
  (null (free-vars type)))

(defclass simple-type ()
  ((llvm :initarg :llvm :reader llvm)))

(defclass machine-int ()
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

(defclass function-type ()
  ((arg-types :initarg :arg-types :accessor arg-types)
   (return-type :initarg :return-type :accessor return-type)))

(defmethod free-vars ((type function-type))
  (append (free-vars (return-type type))
	  (mapcar #'free-vars (arg-types type))))

(defmethod bl-type= ((a function-type) (b function-type))
  (and (every #'bl-type= (arg-types a) (arg-types b))
       (bl-type= (return-type a) (return-type b))))

(defmethod print-object ((type function-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~A~^ ~} -> ~A" (arg-types type) (return-type type))))
