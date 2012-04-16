(in-package #:bitlisp)

(defgeneric unify (a b)
  (:documentation "Find the most general unifier for two types")
  (:method (a b)
    (declare (ignore a b))
    (error "structural mismatch")))

(defmethod unify :around (a b)
  (handler-bind ((simple-error #'(lambda (e) (error "Unable to unify ~A with ~A: ~A" a b e))))
    (call-next-method)))

(defmethod unify ((a list) (b list))
  (let ((subst (unify (first a) (first b))))
    (subst-compose (unify (subst-apply subst (rest a))
                          (subst-apply subst (rest b)))
                   subst)))

(defmethod unify ((a tyapp) (b tyapp))
  (loop :with subst := nil
        :for ta :in (cons (operator a) (args a))
        :for tb :in (cons (operator b) (args b))
        :do (setf subst (subst-compose (unify (subst-apply subst ta)
                                              (subst-apply subst tb))
                                       subst))
        :finally (return subst)))

(defmethod unify ((a tyvar) b)
  (cond ((bl-type= a b) nil)
        ((member a (free-vars b))
         (error "Recursion check fails"))
        ((/= (kind a) (kind b))
         (error "Kind mismatch: ~A ≠ ~A"
                (kind a) (kind b)))
        (t (make-subst a b))))

(defmethod unify (a (b tyvar))
  (unify b a))

(defmethod unify ((a tycon) (b tycon))
  (if (bl-type= a b)
      nil
      (error "Constructor mismatch")))

(defmethod unify ((a pred) (b pred))
  (if (eq (interface a) (interface b))
      (unify (args a) (args b))
      (error "Interface mismatch: ~S ≠ ~S" (interface a) (interface b))))

(defmethod unify ((a integer) (b integer))
  (if (= a b)
      nil
      (error "unequal")))


(defun subst-merge (sa sb)
  (if (every (lambda (v) (bl-type= (subst-apply sa v)
                                   (subst-apply sb v)))
             (intersection (mapcar #'car sa)
                           (mapcar #'car sb)))
      (append sa sb)
      (error "merge fails")))


(defgeneric match (a b)
  (:documentation "Given two types t1 and t2, the goal of matching is to find a substitution s such that (bl-type= t2 (subst-apply s t1)).")
  (:method (a b)
    (declare (ignore a b))
    (error "fell through all cases")))

(defmethod match :around (a b)
  (handler-bind ((simple-error (lambda (e) (error "Unable to match ~A with ~A: ~A" a b e))))
    (call-next-method)))

(defmethod match ((a list) (b list))
  (reduce #'subst-merge (mapcar #'match a b)))

(defmethod match ((a tyapp) (b tyapp))
  (reduce #'subst-merge (cons (match (operator a) (operator b))
                              (mapcar #'match (args a) (args b)))))

(defmethod match ((a tyvar) b)
  (if (= (kind a) (kind b))
      (make-subst a b)
      (error "kind mismatch: ~A ≠ ~A" (kind a) (kind b))))

(defmethod match ((a tycon) (b tycon))
  (if (bl-type= a b)
      nil
      (error "constructor mismatch")))

(defmethod match ((a pred) (b pred))
  (if (eq (interface a) (interface b))
      (match (args a) (args b))
      (error "interface mismatch: ~A ≠ ~A" (interface a) (interface b))))
