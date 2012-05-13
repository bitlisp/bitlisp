(in-package #:bitlisp)

(defclass environment ()
  ((value-bindings :initform (make-hash-table :test 'eq)
                   :initarg :value-bindings :reader value-bindings)
   (type-bindings :initform (make-hash-table :test 'eq)
                  :initarg :type-bindings :reader type-bindings)
   (interface-bindings :initform (make-hash-table :test 'eq)
                       :initarg :interface-bindings :reader interface-bindings)
   (parents :initform () :initarg :parents :accessor parents)
   (module :initform nil :initarg :module :accessor module)
   (toplevel? :initarg :toplevel? :accessor toplevel?)))

(defmethod print-object ((env environment) stream)
  (print-unreadable-object (env stream :type t)
    (princ (module-fqn (module env)) stream)))

(defun make-subenv (parent)
  (make-instance 'environment
                 :parents (list parent)
                 :module (module parent)
                 :toplevel? (toplevel? parent)))

(defvar *primitives-env* (make-instance 'environment :toplevel? t)
  "Environment in which all primitive operators are bound")

(defun env-bindings (env &optional (namespace :value))
  (ecase namespace
    (:value (value-bindings env))
    (:type (type-bindings env))
    (:interface (interface-bindings env))))

(defun %lookup (symbol &optional (namespace :value) (env *primitives-env*))
  (setf symbol (ensure-bl-sym symbol))
  (if env
      (multiple-value-bind (place exists) (gethash symbol (env-bindings env namespace))
        (if exists
            (values place t)
            (loop for parent in (parents env)
                  do (multiple-value-bind (place exists) (%lookup symbol namespace parent)
                       (when exists
                           (return (values place t)))))))
      (values nil nil)))

(defun lookup (symbol &optional (namespace :value) (env *primitives-env*))
  (multiple-value-bind (binding exists) (%lookup symbol namespace env)
    (if exists binding
        (error "~A names no ~A in ~A" symbol namespace env))))

(defun bind (env namespace symbol value)
  "Associate SYMBOL with VALUE in ENV"
  (etypecase symbol
    (string (setf symbol (make-bl-symbol symbol)))
    (bl-symbol))
  (multiple-value-bind (old-value exists) (gethash symbol (env-bindings env))
    (when exists
      (warn "Rebinding ~A to ~A (was ~A) in ~A" symbol value old-value env)))
  (setf (gethash symbol (env-bindings env namespace)) value))

(defun unbind (env symbol &optional (namespace :value))
  (remhash symbol (env-bindings env namespace)))

(defun resolve (source &optional (env *primitives-env*))
  "Lower code into internal representations"
  (etypecase source
    (null nil)
    (bl-symbol (lookup source :value env))
    (list
     (destructuring-bind (operator &rest args) source
       (let ((op (resolve operator env)))
	 (typecase op
	   ;(macro (error "TODO: Macroexpansion"))
	   (special-op (apply (special-op-resolver op) env args))
	   (t (cons op (mapcar (rcurry #'resolve env) args)))))))
    ((or string integer single-float double-float) source)))
