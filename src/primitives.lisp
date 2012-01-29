(in-package #:bitlisp)

(defmacro def-bl-type (name class &rest initargs)
  `(bind *core-env* (make-bl-symbol ,name) (make-instance ',class ,@initargs)))

(defmacro def-type-cons (name args &body body)
  `(bind *core-env* (make-bl-symbol ,name)
	 (make-instance 'type-constructor :func (lambda ,args ,@body))))

(defmacro defspecial (name &rest phases)
  `(bind *core-env* (make-bl-symbol ,name)
	 (make-instance 'special-op :name ,name ,@phases)))


(def-bl-type "float" simple-type :llvm (llvm:float-type))
(def-bl-type "double" simple-type :llvm (llvm:double-type))
;;; TODO: 64-bit support
(def-bl-type "word" machine-int  :bits 32 :signed t)
(def-bl-type "uword" machine-int :bits 32 :signed nil)
(def-bl-type "byte" machine-int  :bits 8 :signed t)
(def-bl-type "ubyte" machine-int :bits 8 :signed nil)
(def-bl-type "bool" machine-int  :bits 1 :signed nil)

(def-type-cons "->" (return-type &rest arg-types)
  (make-instance 'function-type :arg-types arg-types :return-type return-type))

(defspecial "the"
    :resolver (lambda (env type form)
		(annotate (type-eval type env) (resolve form env)))
  :typer (lambda (vs type form)
	   (annotate type (type-tag form vs)))
  :constrainer (lambda (type form)
		 (multiple-value-bind (constraints inner-type) (constraints form type)
		   (values (cons (cons type inner-type)
				 constraints)
			   type)))
  :compiler (lambda (type form)
	      (declare (ignore type))
	      (compile form)))

(defspecial "interface"
    :resolver (lambda (env name type-param-names bounds &rest clauses)
		(let* ((obj (make-instance 'interface
					   :name name
					   :bindings (mapcar #'first clauses)))
		       (subenv (make-env :parents (list env)))
		       (type-params (loop for name in type-param-names
					  collect (make-instance 'bounded-type
								 :interface obj
								 :name name))))
		  (loop for name in type-param-names
			for type in type-params
			do (bind subenv name type))
		  (setf (bounds obj) (mapcar (rcurry #'type-eval subenv)
					     bounds)
			(types obj) (mapcar (compose (rcurry #'type-eval subenv)
						     #'second)
					    clauses))
		  (bind env name obj)
		  (loop for name in (mapcar #'first clauses)
			for type in (types obj)
			do (bind env name
				 (make-instance 'interface-func
						:bl-type type
						:interface obj)))
		  (values))))
