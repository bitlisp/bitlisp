(in-package #:bitlisp)

(defclass form ()
  ((type :initarg :type :initform nil :accessor type)))

(defmethod print-object :around ((form form) stream)
  (print-unreadable-object (form stream :type t)
    (if (type form)
        (progn
          (format stream "(the ~A " (type form))
          (call-next-method)
          (princ ")" stream))
        (call-next-method))))

(defgeneric subst-apply (substitution thing)
  (:documentation "Applies SUBSTITUTION to all types in THING.")
  (:method (substitution (thing form))
    (setf (type thing) (subst-apply substitution (type thing)))))

;;; Unification vars are represented as ints for now
(defun make-vargen ()
  (let ((x 0))
    (lambda () (prog1 x (incf x)))))

;;; Takes a form, assigns type vars to the form and all children, and
;;; returns constraint set in the domain of those vars.
(defgeneric constraint-gen (var-generator form)
  ;; FIXME: This probably shouldn't be necessary.
  (:method (var-generator (form t))
    (declare (ignore var-generator))
    nil))

(defclass variable (form)
  ((symbol :initarg :symbol :accessor symbol)))

(defmethod print-object ((var variable) stream)
  (princ (symbol var) stream))

(defmethod constraint-gen (var-generator (form variable))
  (unless (type form)
    (setf (type form) (funcall var-generator)))
  nil)

(defclass application (form)
  ((operator :initarg :operator :accessor operator)
   (args :initarg :args :accessor args)))

(defmethod print-object ((app application) stream)
  (format stream "(~A ~{~A~^ ~})" (operator app) (args app)))

(defmethod constraint-gen (var-generator (form application))
  (let ((constraints (append (constraint-gen var-generator (operator form))
                             (mapcan (curry #'constraint-gen var-generator) (args form))))
        (optype (type (operator form))))
    (check-type optype constructed-type)
    ;; TODO: Real function constructor
    (assert (eq :func (constructor optype)) ()
            "~A (of type ~A) cannot be used as a function."
            (operator form) optype)
    (setf (type form) (first (args optype)))
    (nconc (mapcar #'cons (rest (args optype)) (mapcar #'type (args form)))
           constraints)))

(defclass abstraction (form)
  ((params :initarg :params :accessor params)
   (body :initarg :body :accessor body)
   (env :initarg :env :accessor env)))

(defmethod print-object ((abs abstraction) stream)
  (format stream "(lambda ~A ~{~A~})" (params abs) (body abs)))

(defmethod constraint-gen (var-generator (form abstraction))
  (prog1 (mapcan (curry #'constraint-gen var-generator) (body form))
    (setf (type form) (make-instance 'constructed-type
                                     ;; TODO: Real function constructor
                                     :constructor :func
                                     :args (list* (type (car (last (body form))))
                                                  (mapcar #'type (params form)))))
    nil))

(defclass conditional (form)
  ((condition :initarg :condition :accessor condition)
   (then :initarg :then :accessor then)
   (else :initarg :else :accessor else)))

(defmethod print-object ((con conditional) stream)
  (format stream "(if ~A ~A ~A)" (condition con) (then con) (else con)))

(defmethod constraint-gen (var-generator (form conditional))
  (let ((constraints (nconc (constraint-gen var-generator (condition form))
                            (constraint-gen var-generator (then form))
                            (constraint-gen var-generator (else form)))))
    (setf (type form) (type (then form)))
    (list* (cons (type (then form)) (type (else form)))
           (cons (type (condition form)) (lookup "Bool"))
           constraints)))

(defclass literal (form)
  ((value :initarg :value :accessor value)))

(defmethod print-object ((lit literal) stream)
  (princ (value lit) stream))

;;; TODO: Polymorphic literals
(defmethod constraint-gen (var-generator (form literal))
  (declare (ignore var-generator))
  (setf (type form)
        (etypecase (value form)
          (single-float (lookup "Float"))
          (double-float (lookup "Double"))
          (integer (lookup "Word"))
          (string (error "TODO"))))
  nil)

(defun make-ast (code &optional (env *core-env*))
  (typecase code
    (list (destructuring-bind (operator-src &rest arg-srcs) code
            (let ((operator (make-ast operator-src)))
             (cond
               ((eq operator (lookup (sym "lambda")))
                (let ((child-env (make-env :name "lambda"
                                           :parents (when env
                                                      (list env))))
                      (params (first arg-srcs)))
                  (loop for param in params do
                    (bind child-env param
                          (make-instance 'variable
                                         :symbol param)))
                  (make-instance 'abstraction
                                 :params (mapcar (rcurry #'lookup child-env) params)
                                 :body (mapcar (rcurry #'make-ast child-env)
                                               (rest arg-srcs))
                                 :env child-env)))
               ((eq operator (lookup (sym "if")))
                (assert (null (cdddr arg-srcs)))
                (make-instance 'conditional
                               :condition (make-ast (first arg-srcs))
                               :then (make-ast (second arg-srcs))
                               :else (make-ast (third arg-srcs))))
               (t
                (make-instance 'application
                               :operator operator
                               :args (mapcar (rcurry #'make-ast env)
                                             arg-srcs)))))))
    (bl-symbol (lookup code env))
    (t (make-instance 'literal :value code))))
