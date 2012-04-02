(in-package #:bitlisp)

(defstruct special-op name resolver constrainer subst codegen)

(defmethod print-object ((value special-op) stream)
  (print-unreadable-object (value stream :type t)
    (princ (special-op-name value) stream)))

(defclass var ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (var-type :initarg :var-type :accessor var-type)
   (instantiator :initarg :instantiator :accessor instantiator)
   (llvm :initarg :llvm :accessor llvm)))


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

(defun instantiate-value (utype code &rest types)
  ;; TODO: Check that the concrete types fulfill the constraints
  (let ((subst (reduce 'subst-compose
                       (mapcar 'make-subst
                               (variables utype) types))))
    (unif-apply subst (make-form (inner-type utype) code))))
