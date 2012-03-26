(in-package #:bitlisp)

(defclass protocol-constraint ()
  ((protocol :initarg :protocol :accessor protocol)
   (args :initarg :args :accessor args)))

;;; Substitutions are alists of the form ((var1 . value1) (var2 . value2))
(defun make-subst (var value)
  "Create a substitution consisting of a single replacement of the form var |-> value"
  (list (cons var value)))

(defun subst-compose (sigma gamma)
  "Compose two substitutions (see TAPL 22.1.1)"
  (nconc (loop :for (var . value) :in gamma
               :nconc (make-subst var (subst-apply sigma value)))
         (remove-if (lambda (x) (find x gamma :key #'car))
                    sigma)))

(defun subst-constraints (substitutions constraints)
  (mapcar (lambda (constraint)
            (destructuring-bind (l . r) constraint
              (cons (subst-apply substitutions l)
                    (subst-apply substitutions r))))
          constraints))

;;; Returns list of substitutions and quantifiers.
;;; TODO: Protocols
(defun unify (constraints)
  (when constraints
    (destructuring-bind ((left . right) &rest rest) constraints
      (cond
        ((bl-type= left right)
         ;; Ignore interchangable types
         (unify rest))
        ((and (typevar? left)
              (not (find left (free-vars right))))
         (let ((subst (make-subst left right)))
           (subst-compose (unify (subst-constraints subst rest))
                          subst)))
        ((and (typevar? right)
              (not (find right (free-vars left))))
         (let ((subst (make-subst right left)))
           (subst-compose (unify (subst-constraints subst rest))
                          subst)))
        ((and (typep left 'constructed-type)
              (typep right 'constructed-type)
              (= (length (args left))
                 (length (args right))))
         (unify (nconc (list (cons (constructor left) (constructor right)))
                       (mapcar #'cons (args left) (args right))
                       rest)))))))

(defun unif-apply (unifier form)
  (if (consp form)
      (destructuring-bind (type . code) form
        (make-form (subst-apply unifier type)
                   (if (listp code)
                       (destructuring-bind (operator &rest args) code
                         (typecase operator
                           (special-op (apply (special-op-subst operator) unifier args))
                           (t (mapcar (curry #'unif-apply unifier) code))))
                       code)))
      form))
