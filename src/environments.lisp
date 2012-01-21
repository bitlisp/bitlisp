(in-package #:bitlisp)

(defclass environment ()
  ((name :initform "anonymous" :initarg :name :accessor name)
   (bindings :initform (make-hash-table :test 'eq)
             :initarg :bindings :reader bindings)
   (parents :initform () :initarg :parents :accessor parents)
   (id :initarg :id :reader id)))

(defclass stackframe (environment)
  ((owner :initarg :owner :reader owner)))

(defvar *env-id-table* (make-array '(128)
                                   :element-type 'environment
                                   :adjustable t
                                   :fill-pointer 0))

(defun make-env (&key (name "anonymous") parents (function nil function-supplied))
  (let ((obj (if function-supplied
                 (make-instance 'stackframe
                                :id (fill-pointer *env-id-table*)
                                :name name
                                :parents parents
                                :owner function)
                 (make-instance 'environment
                                :id (fill-pointer *env-id-table*)
                                :name name
                                :parents parents))))
    (vector-push-extend obj *env-id-table*)
    obj))

(defvar *root-env* (make-env :name "root"))

(defun lookup (symbol env)
  (if env
      (multiple-value-bind (value exists) (gethash symbol (bindings env))
        (if exists
            (values value t)
            (loop for parent in (parents env)
                  do (multiple-value-bind (value exists) (lookup symbol parent)
                       (if exists
                           (values value t)
                           (values nil nil))))))
      (values nil nil)))

(defun bind (env symbol value)
  (multiple-value-bind (old-value exists) (gethash symbol (bindings env))
    (when exists
      (warn "Rebinding ~A to ~A (was ~A) in ~A" symbol value old-value env)))
  (setf (gethash symbol (bindings env)) value))
