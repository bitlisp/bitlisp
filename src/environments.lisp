(in-package #:bitlisp)

(defclass environment ()
  ((bindings :initform (make-hash-table :test 'eq)
             :initarg :bindings :reader bindings)
   (parents :initform () :initarg :parents :accessor parents)
   (module :initform nil :initarg :module :accessor module)
   (toplevel? :initarg :toplevel? :accessor toplevel?)))

(defun make-subenv (parent)
  (make-instance 'environment
                 :parents (list parent)
                 :module (module parent)
                 :toplevel? (toplevel? parent)))

(defvar *primitives-env* (make-instance 'environment :toplevel? t)
  "Environment in which all primitive operators are bound")

(defun lookup (symbol &optional (env *primitives-env*))
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
  (etypecase symbol
    (string (setf symbol (make-bl-symbol symbol)))
    (bl-symbol))
  (multiple-value-bind (old-value exists) (gethash symbol (bindings env))
    (when exists
      (warn "Rebinding ~A to ~A (was ~A) in ~A" symbol value old-value env)))
  (setf (gethash symbol (bindings env)) value))

(defun unbind (env symbol)
  (remhash symbol (bindings env)))

(defun resolve (env source)
  "Lower code into internal representations"
  (etypecase source
    (null nil)
    (bl-symbol
     (multiple-value-bind (place exists) (lookup source env)
       (if exists
	   place
	   (error "~A names no binding" source))))
    (list
     (destructuring-bind (operator &rest args) source
       (let ((op (resolve env operator)))
	 (typecase op
	   ;(macro (error "TODO: Macroexpansion"))
	   (special-op (apply (special-op-resolver op) env args))
	   (t (cons op (mapcar (curry #'resolve env) args)))))))
    ((or string integer single-float double-float) source)))
