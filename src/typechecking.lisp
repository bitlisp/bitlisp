(in-package #:bitlisp)

(let ((symbol (make-bl-symbol "the")))
  (defun annotate (type form)
    (list (lookup symbol) type form))

  (defun annotation? (form)
    (and (listp form)
	 (eq (first form) (lookup symbol))))

  (defun anno-type (annotation)
    (second annotation))
  (defun anno-form (annotation)
    (third annotation)))

;;; Substitutions are alists of the form ((var1 . value1) (var2 . value2))
(defun subst-compose (a b)
  (append (mapcar (lambda (cell)
                    (cons (car cell) (subst-apply a (cdr cell))))
                  b)
          (remove-if (lambda (var)
                       (loop for (bvar . replacement) in b
                             when (eq var bvar) return t
                             finally (return nil)))
                     a :key #'car)))

(defun subst-apply (subst thing)
  (cond
    ((not (consp thing))
     (if-let (clause (find thing subst :key #'car))
       (cdr clause)
       thing))
    ((consp (cdr thing)) (mapcar (curry #'subst-apply subst) thing))
    (t (cons (subst-apply subst (car thing)) (subst-apply subst (cdr thing))))))

(defun make-subst (var value)
  (list (cons var value)))

(defun unify (constraints)
  (if (null constraints)
      nil
      (destructuring-bind ((left . right) &rest rest) constraints
        (cond
          ((equal left right)
           (unify rest))
          ((and (typevar? left)
                (not (find left (free-vars right))))
           (let ((subst (make-subst left right)))
             (subst-compose (unify (subst-apply subst rest))
                            subst)))
          ((and (typevar? right)
                (not (find right (free-vars left))))
           (let ((subst (make-subst right left)))
             (subst-compose (unify (subst-apply subst rest))
                            subst)))
	  (t (let ((subst (unify-types left right)))
	       (subst-compose (unify (subst-apply subst rest))
			      subst)))
          (t (error "Unification failed on ~A = ~A" left right))))))

(defun make-typevar (id)
  id)

(defun typevar? (x)
  (integerp x))

(defun type-tag (form &optional (varstream (let ((x 0)) (lambda () (prog1 (make-typevar x) (incf x))))))
  (cond ((and (listp form)
              (typep (car form) 'special-op))
         (apply (typer (car form)) varstream (cdr form)))
        (t (annotate (funcall varstream)
		     (typecase form
		       (list (mapcar (rcurry #'type-tag varstream) form))
		       (t form))))))

(defun constraints (form &optional this-type)
  (etypecase form
    (null nil)
    (list (destructuring-bind (op &rest args) form
	    (cond
	      ((typep op 'special-op)
	       (apply (constrainer op) args))
	      ((annotation? op)
	       (let (argcons argtypes)
		 (loop for arg in args
		       do (multiple-value-bind (cons type) (constraints arg)
			    (setf argcons (append cons argcons))
			    (push type argtypes)))
		 (multiple-value-bind (opcons optype) (constraints op)
		   (values (cons (cons optype (make-instance 'function-type
							     :arg-types argtypes
							     :return-type this-type))
				 (append opcons argcons))
			   this-type))))
	      (t (error "Invalid function call: ~A" form)))))
    (t (values nil (bl-type form)))))

(defun type-subst (substitution form)
  (destructuring-bind (var . value) substitution
    (cond
      ((eql var form) value)
      ((or (typep form 'value) (typep form 'place))
       (setf (bl-type form) (type-subst substitution (bl-type form)))
       form)
      ((listp form) (mapcar (curry #'type-subst substitution) form))
      (t (assign-type var value form)))))

(defun unif-apply (unifier form)
  (if unifier
      (destructuring-bind (substitution &rest rest) unifier
        (unif-apply rest (type-subst substitution form)))
      form))

(defun infer (form)
  (let ((tagged (type-tag form)))
    (unif-apply (unify (constraints tagged)) tagged)))
