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

(defvar *core-env* (make-env :name "core"))

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

(defun bind (env symbol place)
  "Associate SYMBOL with storage location PLACE"
  (check-type symbol bl-symbol)
  (multiple-value-bind (old-place exists) (gethash symbol (bindings env))
    (when exists
      (warn "Rebinding ~A to ~A (was ~A) in ~A" symbol place old-place env)))
  (setf (gethash symbol (bindings env)) place))

(defun unbind (env symbol)
  (remhash symbol (bindings env)))

(defun resolve (form &optional (env *core-env*))
  "Lower code into internal representations"
  (typecase form
    (null nil)
    ((or bl-symbol string)
     (multiple-value-bind (place exists) (lookup form env)
       (if exists
	   place
	   (error "~A names no binding" form))))
    (list
     (destructuring-bind (operator &rest args) form
       (let ((op (resolve operator env)))
	 (typecase op
	   (macro (error "TODO: Macroexpansion"))
	   (special-op (apply (resolver op) env args))
	   (t (cons op (mapcar (rcurry #'resolve env) args)))))))
    (integer
     (let ((word-type (lookup "word")))
       (make-instance 'value
		      :contents form
		      :bl-type word-type
		      :llvm (llvm:const-int (llvm word-type) form))))
    (single-float
     (let ((float-type (lookup "float")))
       (make-instance 'value
		      :contents form
		      :bl-type float-type
		      :llvm (llvm:const-real (llvm float-type) form))))
    (double-float
     (let ((float-type (lookup "double")))
       (make-instance 'value
		      :contents form
		      :bl-type float-type
		      :llvm (llvm:const-real (llvm float-type) form))))))
