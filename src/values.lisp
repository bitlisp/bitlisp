(in-package #:bitlisp)

(defstruct special-op name resolver inferrer codegen)

(defmethod print-object ((value special-op) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defclass value ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (value-type :initarg :value-type :accessor value-type)
   (instances :initform nil :accessor instances
              :documentation "(bl-type . llvm) alist for cached compiled instances of polymorphic values")
   (llvm :initarg :llvm :accessor llvm)))

(defclass prim-poly-value (value) ())

(defmethod print-object ((value value) stream)
  (print-unreadable-object (value stream :type t)
    (princ (name value) stream)))

(deftype form () 'cons)

(defun make-form (type code)
  (cons type code))

(defun form-type (form)
  (car form))

(defun form-code (form)
  (cdr form))
