(in-package #:bitlisp)

(defclass form ()
  ((type :initarg :type :accessor type)))

(defclass variable (form)
  ((symbol :initarg :symbol :accessor symbol)))

(defclass application (form)
  ((operator :initarg :operator :accessor operator)
   (args :initarg :args :accessor args)))

(defclass abstraction (form)
  ((params :initarg :params :accessor params)
   (body :initarg :body :accessor body)
   (env :initarg :env :accessor env)))

(defclass conditional (form)
  ((condition :initarg :condition :accessor condition)
   (then :initarg :then :accessor then)
   (else :initarg :else :accssor else)))

(defclass literal (form)
  ((value :initarg :value :accessor value)))

(defun make-ast (code &optional (env *core-env*))
  (typecase code
    (list (destructuring-bind (operator &rest args) code
            ;; TODO: Look up special operators by value, not name.
            (cond
              ((eq operator (sym "lambda"))
               (let ((child-env (make-env :name "lambda"
                                          :parents (when env
                                                     (list env))))
                     (params (first args)))
                 (loop for param in params do
                   (bind child-env param
                         (make-instance 'variable
                                        :symbol param)))
                (make-instance 'abstraction
                               :params params
                               :body (mapcar (rcurry #'make-ast child-env)
                                             (rest args))
                               :env child-env)))
              ((eq operator (sym "if"))
               (make-instance 'conditional
                              :condition (make-ast (first args))
                              :then (make-ast (second args))
                              :else (make-ast (third args))))
              (t
               (make-instance 'application
                              :operator (make-ast operator)
                              :args args)))))
    (bl-symbol (lookup code env))
    (t (make-instance 'literal :value code))))
