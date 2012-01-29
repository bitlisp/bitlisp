(in-package #:bitlisp)

(defgeneric free-vars (type))
(defgeneric conrete-type (type))
(defgeneric bl-type= (a b))
(defgeneric unify-types (a b))

(defmethod free-vars ((type t))
  nil)
(defmethod concrete-type ((type t))
  t)
(defmethod bl-type= ((a t) (b t))
  (eql a b))
(defmethod unify-types ((a t) (b t))
  (if (bl-type= a b)
      a
      (error "Don't know how to unify ~A with ~A" a b)))


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

(defmethod unify-types ((left function-type) (right function-type))
  (unless (= (length (arg-types left)) (length (arg-types right)))
    (error "Unification failed: mismatched arity: ~A and ~A" left right))
  (unify (cons (cons (return-type left)
		     (return-type right))
	       (mapcar #'cons (arg-types left) (arg-types right)))))

(defmethod print-object ((type function-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~{~A~^ ~} -> ~A" (arg-types type) (return-type type))))


(defclass interface ()
  ((name :initarg :name :accessor name)
   (bounds :initarg :bounds :accessor bounds)
   (bindings :initarg :bindings :accessor bindings)
   (types :initarg :types :accessor types)))

(defmethod print-object ((obj interface) stream)
  (princ (name obj) stream))

(defun interface> (a b)
  (if-let (bounds (bounds b))
    (let ((super-ifs (mapcar #'first bounds)))
      (or (find a super-ifs)
	  (some (curry #'interface> a) super-ifs)))
    nil))

(defun most-specific (a b)
  (if (interface> a b)
      b
      a))


(defclass bounded-type ()
  ((interface :initarg :interface :accessor interface)
   (name :initarg :name :reader name)
   (var :initarg :var :reader var)))

(defmethod print-object ((type bounded-type) stream)
  (print-unreadable-object (type stream :type t)
    (format stream "~A ~A"
	    (interface type)
	    (if (slot-boundp type 'var)
		(var type)
		(name type)))))

(defun type-eval (form env)
  (typecase form
    (list (destructuring-bind (cname &rest args) form
	    (let ((constr (lookup cname env)))
	      (etypecase constr
		(interface (list* constr (mapcar (rcurry #'type-eval env) args)))
		(type-constructor
		 (apply (func constr) (mapcar (rcurry #'type-eval env) args)))))))
    (bl-symbol (lookup form env))
    (t form)))

(defmethod unify-types ((a bounded-type) (b bounded-type))
  (if-let (specific (most-specific a b))
    (list* (cons a specific)
	   (cons b specific)
	   (unify (cons (var a) (var b))))
    (unify (cons (var a) (var b)))))

(defmethod unify-types ((a bounded-type) (b t))
  (if (implements? b (interface a))
      b
      (error "~A does not implement ~A" b (interface a))))

(defmethod unify-types ((a t) (b bounded-type))
  (unify-types b a))
