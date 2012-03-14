(in-package #:bitlisp)

(defclass form ()
  ((type :initarg :type :initform nil :accessor type)))

;;; Takes a form, assigns type vars to the form and all children,
;;; and returns constraint set in the domain of those vars.
(defgeneric constraint-gen (var-generator form)
  ;; FIXME: This probably shouldn't be necessary.
  (:method (var-generator (form t))
    (declare (ignore var-generator))
    nil))

(defclass variable (form)
  ((symbol :initarg :symbol :accessor symbol)))

(defmethod constraint-gen (var-generator (form variable))
  (unless (type form)
    (setf (type form) (funcall var-generator)))
  nil)

(defclass application (form)
  ((operator :initarg :operator :accessor operator)
   (args :initarg :args :accessor args)))

(defmethod constraint-gen (var-generator (form application))
  (let ((constraints (append (constraint-gen var-generator (operator form))
                             (mapcan (curry #'constraint-gen var-generator) (args form))))
        (optype (type (operator form))))
    (check-type optype function-type)
    (setf (type form) (return-type optype))
    (nconc (mapcar #'cons (arg-types optype) (mapcar #'type (args form)))
           constraints)))

(defclass abstraction (form)
  ((params :initarg :params :accessor params)
   (body :initarg :body :accessor body)
   (env :initarg :env :accessor env)))

(defmethod constraint-gen (var-generator (form abstraction))
  (prog1 (mapcan (curry #'constraint-gen var-generator) (body form))
    (setf (type form) (make-instance 'function-type
                                     :return-type (type (car (last (body form))))
                                     :arg-types (mapcar #'type (params form))))))

(defclass conditional (form)
  ((condition :initarg :condition :accessor condition)
   (then :initarg :then :accessor then)
   (else :initarg :else :accessor else)))

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
