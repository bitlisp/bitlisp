(in-package #:bitlisp)

(defstruct special-op name resolver inferrer codegen)

(defmethod print-object ((value special-op) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defclass var ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (var-type :initarg :var-type :accessor var-type)
   (instances :initform nil :accessor instances
              :documentation "(bl-type . llvm) alist for cached compiled instances of polymorphic values")
   (llvm :initarg :llvm :accessor llvm)))

(defclass prim-poly-var (var) ())

(defmethod print-object ((value var) stream)
  (print-unreadable-object (value stream :type t)
    (princ (name value) stream)))

(deftype form () 'cons)

(defun make-form (type code)
  (cons type code))

(defun form-type (form)
  (car form))

(defun form-code (form)
  (cdr form))
