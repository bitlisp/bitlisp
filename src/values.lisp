(in-package #:bitlisp)

(defstruct special-op name resolver inferrer codegen)

(defmethod print-object ((value special-op) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defclass value ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (value-type :initarg :value-type :accessor value-type)
   (llvm :initarg :llvm :accessor llvm)))

(defclass poly-value (value)
  ((instances :initform nil :accessor instances
              :documentation "(bl-type . llvm) alist for cached compiled instances of polymorphic values")))

(defclass prim-poly-value (poly-value) ()
  (:documentation "Like a regular value, except the llvm slot holds a function that takes an LLVM module and the instantiation type"))

(defclass regular-poly-value (poly-value)
  ((form :initarg :form :reader form)))

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
