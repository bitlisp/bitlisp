(in-package #:bitlisp)

(defun infer-literal (lit)
  (etypecase lit
    (character (make-form (lookup "codepoint" :type) lit))
    ;; TODO: Polymorphic strings because why not?
    (string (make-form (type-eval '("ptr" "byte")) lit))
    (integer (let ((var (make-instance 'tyvar :kind 1)))
               (values (make-form var lit)
                       (list (make-instance 'pred
                                            :interface
                                            (if (= lit 0)
                                                (or (lookup "abelian-group" :interface)
                                                    (error "abelian-group undefined"))
                                                (or (lookup "ring" :interface)
                                                    (error "ring undefined")))
                                            :args (list var))))))
    (ratio (let ((var (make-instance 'tyvar :kind 1)))
             (values (make-form var lit)
                     (list (make-instance 'pred
                                          :interface
                                          (or (lookup "division-ring" :interface)
                                              (error "division-ring undefined"))
                                          :args (list var))))))))

(defun infer-expr-seq (exprs)
  (loop :with forms := nil
        :with preds := nil
        :with subst := nil
        :for expr :in exprs
        :do (multiple-value-bind (form predicate substitution)
                (infer-expr expr)
              (push form forms)
              (push predicate preds)
              (setf subst (subst-compose substitution subst)))
        :finally
           (return (values (nreverse forms) (apply #'nconc preds) subst))))

(defun infer-app (operator args)
  (multiple-value-bind (funcform fpreds fsubst) (infer-expr operator)
    (multiple-value-bind (argforms preds subst) (infer-expr-seq args)
      (values (make-form (second (args (form-type funcform)))
                         (cons funcform argforms))
              (nconc fpreds preds)
              (subst-compose (unify (apply #'make-prodty
                                           (mapcar #'form-type argforms))
                                    (first (args (form-type funcform))))
                             (subst-compose subst fsubst))))))

(defun infer-expr (expr)
  "(values form preds subst)"
  (typecase expr
    (value (let ((qualty (fresh-instance (value-type expr))))
             (values (make-form (head qualty) expr)
                     (copy-list (context qualty))
                     nil)))
    (list (destructuring-bind (operator &rest args) expr
            (typecase operator
              (special-op (apply (special-op-inferrer operator) args))
              (t (infer-app operator args)))))
    (t (infer-literal expr))))

(defun infer-source (source)
  (infer-expr (resolve source)))

(defun split-preds (fixed-vars quant-vars preds)
  "(values deferred retained)"
  (declare (ignore quant-vars))         ;Only need these for defaulting, which is TODO
  (let ((reduced (reduce-context preds)))
    (partition (lambda (pred)
                 (every (rcurry #'member fixed-vars)
                        (free-vars pred)))
               reduced)))

(defun infer-kinds (resolved-type &optional (arg-count 0))
  "Takes a resolved type form and returns a (tyvar/tygen . kind) alist."
  (assert (not (null resolved-type)))
  (etypecase resolved-type
    ((or tyvar tygen) (list (cons resolved-type (1+ arg-count))))
    (list (apply #'nconc
                 (infer-kinds (first resolved-type) (length (rest resolved-type)))
                 (mapcar #'infer-kinds (rest resolved-type))))
    ((or bl-type integer) nil)))
