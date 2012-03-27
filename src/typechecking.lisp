(in-package #:bitlisp)

(defun build-types (code)
  (let ((resolved (resolve *core-env* code)))
    (multiple-value-bind (form constraints)
        (constrain (make-vargen) resolved)
      (unif-apply (unify constraints) form))))

(defun make-vargen ()
  (let ((x 0))
    (lambda () (prog1 x (incf x)))))

(defun generalize-type (incomplete-type)
  (if-let (vars (free-vars incomplete-type))
    (make-instance 'quantified-type
                   :variables (free-vars incomplete-type)
                   :inner-type incomplete-type)
    incomplete-type))

(defun constrain (vargen form)
  (if (atom form)
      (etypecase form
        (var (make-form (type form) form))
        (integer (make-form (lookup "Word") form))
        (single-float (make-form (lookup "Float") form))
        (double-float (make-form (lookup "Double") form)))
      (destructuring-bind (operator &rest args) form
        (typecase operator
          (special-op (apply (special-op-constrainer operator) vargen args))
          (t (loop :with constraints := nil
                   :with forms := nil
                   :with rtype := (funcall vargen)
                   :for arg :in args
                   :do (multiple-value-bind (arg-form arg-constraints)
                           (constrain vargen arg)
                         (push arg-constraints constraints)
                         (push arg-form forms))
                   :finally
                      (setf forms (nreverse forms))
                      (multiple-value-bind (op-form op-constrs)
                          (constrain vargen operator)
                        (push op-form forms)
                        (push op-constrs constraints)
                        (return
                          (values
                           (make-form rtype forms)
                           (apply #'nconc
                                  (list (cons (form-type op-form)
                                              (apply #'make-ftype rtype
                                                     (mapcar #'form-type (rest forms)))))
                                  constraints))))))))))

