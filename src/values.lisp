(in-package #:bitlisp)

(defstruct special-op name resolver inferrer codegen)

(defmethod print-object ((value special-op) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defstruct special-tycon name resolver constructor)

(defmethod print-object ((value special-tycon) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defclass value ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (value-type :initarg :value-type :accessor value-type)
   (llvm :initarg :llvm :accessor llvm)
   (instances :initform nil :accessor instances
              :documentation "(bl-type . llvm) alist for cached compiled instances of polymorphic values")
   (code :initarg :code :accessor code)))

(defmethod print-object ((value value) stream)
  (print-unreadable-object (value stream :type t)
    (princ (name value) stream)))

(defclass prim-poly-value (value) ()
  (:documentation "Like a regular value, except the llvm slot holds a function that takes an LLVM module and the instantiation type"))

(defclass interface-value (value)
  ((interface :initarg :interface :reader interface)
   (candidates :initarg :candidates :reader candidates)))

(deftype form () 'cons)

(defun make-form (type code)
  (cons type code))

(defun form-type (form)
  (car form))

(defun form-code (form)
  (cdr form))
