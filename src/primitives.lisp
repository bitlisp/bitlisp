(in-package #:bitlisp)

(defmacro defspecial (name self args
                      (env &body resolver)
                      (vargen &body constrainer)
                      (unifier &body subst))
  `(let ((,self (make-special-op :name ,name)))
     (setf (special-op-resolver ,self) (lambda (,env ,@args) ,@resolver)
           (special-op-constrainer ,self) (lambda (,vargen ,@args) ,@constrainer)
           (special-op-subst ,self) (lambda (,unifier ,@args) ,@subst))
     (bind *core-env* (make-bl-symbol ,name) ,self)))

(defspecial "lambda" self (args &rest body)
    (env
      (let ((new-env (make-env :parents (list env) :toplevel? nil))
            (arg-vars (mapcar (lambda (sym) (make-instance 'var :name sym)) args)))
        (mapc (curry #'bind new-env) args arg-vars)
        (list* self arg-vars
               (mapcar (curry #'resolve new-env) body))))
    (vargen
     ;; TODO: Probably shouldn't mutate here.
      (mapc (lambda (v) (setf (var-type v) (funcall vargen))) args)
      (loop :with constraints := nil
            :with forms := nil
            :for form :in body
            :do (multiple-value-bind (new-form new-constraints)
                    (constrain vargen form)
                  (push new-form forms)
                  (push new-constraints constraints))
            :finally (return
                       (values
                        (make-form
                         (apply #'make-ftype
                                (form-type (car (last forms)))
                                (mapcar #'var-type args))
                         (list* self args (nreverse forms)))
                        (apply #'nconc constraints)))))
    (unifier
      (list* self args (mapcar (curry 'unif-apply unifier) body))))

(defspecial "if" self (condition then else)
    (env
      (list self (resolve env condition)
            (resolve env then) (resolve env else)))
    (vargen
      (multiple-value-bind (cform ccons) (constrain vargen condition)
        (multiple-value-bind (tform tcons) (constrain vargen then)
          (multiple-value-bind (eform econs) (constrain vargen else)
            (values (make-form (form-type tform) (list self cform tform eform))
                    (nconc (list (cons (form-type tform)
                                       (form-type eform)))
                           ccons tcons econs))))))
    (unifier
      (list* self
             (unif-apply unifier condition)
             (unif-apply unifier then)
             (unif-apply unifier else))))

(defspecial "def" self (name value)
    (env
      (let ((var (make-instance 'var :name name)))
        (bind env name var)
        (list self var (resolve env value))))
    (vargen
      (let ((value-type (funcall vargen)))
        ;; TODO: Is it even *possible* not to mutate here?
        (setf (var-type name) value-type)
        (multiple-value-bind (vform vcons)
            (constrain vargen value)
          (let ((actual-type (generalize-type (form-type vform))))
           (values
            (make-form actual-type (list self name vform))
            (cons (cons value-type actual-type) vcons))))))
    (unifier
      (setf (var-type name) (subst-apply unifier (var-type name)))
      (list self name (unif-apply unifier value))))

(defmacro def-bl-type (name class &rest initargs)
  `(bind *core-env* (make-bl-symbol ,name) (make-instance ',class ,@initargs)))

(def-bl-type "Unit" unit-type)
(def-bl-type "Float"  simple-type :llvm (llvm:float-type))
(def-bl-type "Double" simple-type :llvm (llvm:double-type))
;;; TODO: 64-bit support
(def-bl-type "Word"  machine-int :bits 32 :signed t)
(def-bl-type "UWord" machine-int :bits 32 :signed nil)
(def-bl-type "Byte"  machine-int :bits 8  :signed t)
(def-bl-type "UByte" machine-int :bits 8  :signed nil)
(def-bl-type "Bool"  machine-int :bits 1  :signed nil)

(defmacro defprimfun (name type)
  `(bind *core-env* (make-bl-symbol ,name)
         (make-instance 'var :name ,name :var-type ,type)))

(defprimfun "word+" (let ((ty (lookup "Word")))
                      (make-ftype ty ty ty)))

(defprimfun "word>" (let ((ty (lookup "Word")))
                      (make-ftype (lookup "Bool") ty ty)))
