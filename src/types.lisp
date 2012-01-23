(in-package #:bitlisp)

(defgeneric free-vars (type))
(defgeneric conrete-type (type))
(defgeneric bl-type= (a b))

(defmethod free-vars ((type t))
  nil)
(defmethod concrete-type ((type t))
  t)
(defmethod bl-type= ((a t) (b t))
  (eql a b))


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

(defmethod concrete-type ((type function-type))
  (and (every #'concrete-type (arg-types type))
       (concrete-type (return-type type))))

(defmethod bl-type= ((a function-type) (b function-type))
  (and (every #'identity (mapcar #'bl-type= (arg-types a) (arg-types b)))
       (bl-type= (return-type a) (return-type b))))

(defmethod print-object ((type function-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~A~^ ~} -> ~A" (arg-types type) (return-type type))))


(defclass interface ()
  ((name :initarg :name :accessor name)
   (parents :initarg :parents :accessor parents)
   (bindings :initarg :bindings :accessor bindings)
   (types :initarg :types :accessor types)))

(defmethod print-object ((obj interface) stream)
  (princ (name obj) stream))


(defclass bounded-type ()
  ((interface :initarg :interface :accessor interface)
   (name :initarg :name :reader name)))

(defmethod print-object ((type bounded-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~A ~A" (interface type) (name type))))

(defun type-eval (form env)
  (typecase form
    (list (destructuring-bind (cname &rest args) form
	    (let ((constr (lookup cname env)))
	      (apply (func constr) (mapcar (rcurry #'type-eval env) args)))))
    (bl-symbol (lookup form env))
    (t form)))
