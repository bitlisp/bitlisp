(in-package #:bitlisp)

;;; TODO: Operate on binding groups
(defun build-types (env code)
  (multiple-value-bind (resolved next-module) (resolve env code)
    (multiple-value-bind (form constraints predicates)
        (constrain resolved)
      (values (unif-apply (unify constraints) form)
              (simplify predicates)
              next-module))))

(defun generalize-type (inferred-type)
  (let* ((vars (free-vars inferred-type))
         (gens (loop :for n :from 0
                     :for var :in vars
                     :collect (make-instance 'tygen
                                             :number n
                                             :kind (kind var)))))
   (make-instance 'scheme
                  :vars gens
                  :inner-type (subst-apply (mapcar #'cons vars gens) inferred-type))))

;;; TODO: Operate on binding groups
(defun constrain (form)
  "Returns (values form equality-constraints predicates)"
  (if (atom form)
      (etypecase form
        (var (let* ((tyqual (fresh-instance (var-type form))))
               (values (make-form (head tyqual) form)
                       nil
                       (context tyqual))))
        ;; TODO: Polymorphic strings because why not?
        (string (make-form (type-eval '("Ptr" "Byte")) form))
        (character (make-form (lookup "Codepoint") form))
        (integer (let ((var (make-instance 'tyvar :kind 1)))
                   (values (make-form var form)
                           nil
                           (list (make-pred (lookup "Ring") var)))))
        (ratio (let ((var (make-instance 'tyvar :kind 1)))
                 (values (make-form var form)
                         nil
                         (list (make-pred (lookup "EuclidianDomain") var))))))
      (destructuring-bind (operator &rest args) form
        (typecase operator
          (special-op (apply (special-op-constrainer operator) args))
          (t (loop :with constraints := nil
                   :with forms := nil
                   :with rtype := (make-instance 'tyvar :kind 1)
                   :for arg :in args
                   :do (multiple-value-bind (arg-form arg-constraints)
                           (constrain arg)
                         (push arg-constraints constraints)
                         (push arg-form forms))
                   :finally
                      (setf forms (nreverse forms))
                      (multiple-value-bind (op-form op-constrs)
                          (constrain operator)
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
