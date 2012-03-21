(in-package #:bitlisp)

(defclass environment ()
  ((name :initform "anonymous" :initarg :name :accessor name)
   (bindings :initform (make-hash-table :test 'eq)
             :initarg :bindings :reader bindings)
   (parents :initform () :initarg :parents :accessor parents)
   (id :initarg :id :reader id)
   (toplevel? :initarg :toplevel? :accessor toplevel?)))

(defvar *env-id-table* (make-array '(128)
                                   :element-type 'environment
                                   :adjustable t
                                   :fill-pointer 0))

(defun make-env (&key (name "anonymous") toplevel? parents)
  (let ((obj (make-instance 'environment
                            :id (fill-pointer *env-id-table*)
                            :name name
                            :parents parents
                            :toplevel? toplevel?)))
    (vector-push-extend obj *env-id-table*)
    obj))

(defvar *core-env* (make-env :name "core" :toplevel? t))

(defun lookup (symbol &optional (env *core-env*))
  (when (stringp symbol)
    (setf symbol (make-bl-symbol symbol)))
  (if env
      (multiple-value-bind (place exists) (gethash symbol (bindings env))
        (if exists
            (values place t)
            (loop for parent in (parents env)
                  do (multiple-value-bind (place exists) (lookup symbol parent)
                       (when exists
                           (return (values place t)))))))
      (values nil nil)))

(defun bind (env symbol value)
  "Associate SYMBOL with VALUE in ENV"
  (check-type symbol bl-symbol)
  (multiple-value-bind (old-value exists) (gethash symbol (bindings env))
    (when exists
      (warn "Rebinding ~A to ~A (was ~A) in ~A" symbol value old-value env)))
  (setf (gethash symbol (bindings env)) value))

(defun unbind (env symbol)
  (remhash symbol (bindings env)))

(defun resolve (env form)
  "Lower code into internal representations"
  (etypecase form
    (null nil)
    (bl-symbol
     (multiple-value-bind (place exists) (lookup form env)
       (if exists
	   place
	   (error "~A names no binding" form))))
    (list
     (destructuring-bind (operator &rest args) form
       (let ((op (resolve env operator)))
	 (typecase op
	   ;(macro (error "TODO: Macroexpansion"))
	   (special-op (apply (special-op-resolver op) env args))
	   (t (cons op (mapcar (curry #'resolve env) args)))))))
    ((or integer single-float double-float) form)))
