(in-package #:bitlisp)

(defun build-types (module code)
  (multiple-value-bind (resolved next-module) (resolve (env module) code)
    (multiple-value-bind (form constraints)
        (constrain (make-vargen) resolved)
      (values (unif-apply (unify constraints) form) next-module))))

(defun make-vargen ()
  (let ((x 0))
    (lambda ()
      (prog1 (make-instance 'type-var :number x)
        (incf x)))))

(defun generalize-type (incomplete-type)
  (if-let (vars (free-vars incomplete-type))
    (make-instance 'universal-type
                   :variables vars
                   :inner-type incomplete-type)
    incomplete-type))

(defun constrain (vargen form)
  (if (atom form)
      (etypecase form
        (var (let ((type (var-type form)))
               (typecase type
                  (universal-type
                   (multiple-value-bind (local-type constraints)
                       (instantiate-type type vargen)
                     (values (make-form local-type (form-code form))
                             constraints)))
                  (t (make-form type form)))))
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
