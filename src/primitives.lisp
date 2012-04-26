(in-package #:bitlisp)

(defmacro defctor (name (&rest args) &body builder)
  (with-gensyms (sym)
   `(let ((,sym (make-bl-symbol ,name)))
      (bind *primitives-env* :type ,sym
            (make-instance 'tycon
                           :name ,sym
                           :kind ,(1+ (length args))
                           :llvm (lambda ,args ,@builder))))))

(defctor "unit"   () (llvm:void-type))
(defctor "bool"   () (llvm:int1-type))
(defctor "float"  () (llvm:float-type))
(defctor "double" () (llvm:double-type))
(defctor "int" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "uint" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "func" (arg-type return-type)
  (llvm:function-type (llvm return-type)
                      (cond
                        ((prodtype? arg-type)
                         (mapcar #'llvm (flatten-product arg-type)))
                        ((eq arg-type (lookup "unit" :type)) nil)
                        (t (list (llvm arg-type))))))
(defctor "ptr" (inner-type)
  (llvm:pointer-type (llvm inner-type)))
(defctor "vector" (inner-type count)
  (check-type count integer)
  (assert (or (eq inner-type (lookup "bool" :type))
              (and (typep inner-type 'tyapp)
                   (member (operator inner-type)
                           (list (lookup "int" :type)
                                 (lookup "uint" :type)
                                 (lookup "ptr" :type)
                                 (lookup "float" :type)
                                 (lookup "double" :type)))))
          (inner-type)
          "Constructing vectors of type ~A is unsupported" inner-type)
  (llvm:vector-type (llvm inner-type) count))
(defctor "*" (l r)
  (llvm:struct-type
   (mapcar #'llvm (nconc (flatten-product l) (flatten-product r)))
   nil))

(defun ftype? (type)
  (or (and (typep type 'tyapp)
           (eq (lookup "func" :type) (operator type)))
      (and (typep type 'scheme)
           (ftype? (inner-type type)))))

(defun prodtype? (type)
  (or (and (typep type 'tyapp)
           (eq (lookup "*" :type) (operator type)))
      (and (typep type 'scheme)
           (prodtype? (inner-type type)))))

(defun flatten-product (product)
  (if (prodtype? product)
      (destructuring-bind (l r) (args product)
        (nconc (flatten-product l) (flatten-product r)))
      (list product)))

(defparameter *primfun-builders* nil)

(defun build-primfuns (bl-module llvm-module)
  (mapc (lambda (pair)
          (setf (llvm (car pair)) (funcall (cdr pair) bl-module llvm-module)))
        *primfun-builders*))

(defmacro defprimfun (name rtype args (module builder type) &body llvm)
  (with-gensyms (symbol func value bl-module)
    `(let* ((,symbol (make-bl-symbol ,name))
            (,type (make-ftype (make-prodty ,@(mapcar (compose (curry #'list 'lookup) #'second) args))
                               (lookup ,rtype)))
            (,value (make-instance 'value
                                   :name ,symbol
                                   :value-type ,type)))
       (push (cons ,value (lambda (,bl-module ,module)
                            (with-func (,func ,builder ,module ,type
                                        :name (symbol-fqn ,bl-module ,symbol)
                                        :arg-names ',(mapcar (compose #'string-downcase #'first) args))
                              (destructuring-bind ,(mapcar #'first args)
                                  (llvm:params ,func)
                                ,@llvm))))
             *primfun-builders*)
       (bind *primitives-env* :value ,symbol ,value))))

(defmacro defprimpoly (name vars return-type args (builder) &body instantiator)
  (with-gensyms (module sym type qvars subenv func inst-type)
    `(let* ((,sym (make-bl-symbol ,name))
            (n 0)
            (,qvars (mapcar (lambda (v)
                              (declare (ignore v))
                              (make-instance 'tygen
                                             :number (prog1 n (incf n))
                                             :kind 1))
                            ',vars))
            (,type (let ((,subenv (make-subenv *primitives-env*)))
                     (loop :for name :in ',(mapcar #'string vars)
                           :for qvar :in ,qvars
                           :do (bind ,subenv :type name qvar))
                     (destructuring-bind ,vars ',(mapcar #'string vars)
                       (make-instance
                        'scheme
                        :vars ,qvars
                        :inner-type
                        (make-instance
                         'tyqual
                         :context nil
                         :head 
                         (tyapply (lookup "func" :type)
                                  (apply #'make-prodty (mapcar (rcurry #'type-eval ,subenv) (list ,@(mapcar #'second args))))
                                  (type-eval ,return-type ,subenv))))))))
       (bind *primitives-env* :value ,sym
             (make-instance
              'prim-poly-value
              :value-type ,type
              :env *primitives-env*
              :name ,sym
              :llvm
              (destructuring-bind ,vars ,qvars
                (declare (ignorable ,@vars))
               (lambda (,inst-type ,module)
                 (with-func (,func ,builder ,module ,inst-type
                             :name (concatenate 'string
                                                (symbol-fqn nil ,sym)
                                                ":"
                                                (princ-to-string ,inst-type))
                             :linkage :link-once-odr)
                   (destructuring-bind ,(mapcar #'first args) (llvm:params ,func)
                     ,@instantiator)))))))))

(defprimpoly "int+" (a) `("int" ,a) ((x `("int" ,a)) (y `("int" ,a))) (builder)
  (llvm:build-ret (llvm:build-add builder x y "sum")))

(defprimpoly "int-" (a) `("int" ,a) ((x `("int" ,a)) (y `("int" ,a))) (builder)
  (llvm:build-ret (llvm:build-sub builder x y "difference")))

(defprimpoly "int*" (a) `("int" ,a) ((x `("int" ,a)) (y `("int" ,a))) (builder)
  (llvm:build-ret (llvm:build-mul builder x y "product")))

(defprimpoly "int/" (a) `("int" ,a) ((x `("int" ,a)) (y `("int" ,a))) (builder)
  (llvm:build-ret (llvm:build-s-div builder x y "quotient")))

(defprimpoly "int-rem" (a) `("int" ,a) ((x `("int" ,a)) (y `("int" ,a))) (builder)
  (llvm:build-ret (llvm:build-s-rem builder x y "remainder")))

(defprimpoly "uint+" (a) `("uint" ,a) ((x `("uint" ,a)) (y `("uint" ,a))) (builder)
  (llvm:build-ret (llvm:build-add builder x y "sum")))

(defprimpoly "uint-" (a) `("uint" ,a) ((x `("uint" ,a)) (y `("uint" ,a))) (builder)
  (llvm:build-ret (llvm:build-sub builder x y "difference")))

(defprimpoly "uint*" (a) `("uint" ,a) ((x `("uint" ,a)) (y `("uint" ,a))) (builder)
  (llvm:build-ret (llvm:build-mul builder x y "product")))

(defprimpoly "uint/" (a) `("uint" ,a) ((x `("uint" ,a)) (y `("uint" ,a))) (builder)
  (llvm:build-ret (llvm:build-u-div builder x y "quotient")))

(defprimpoly "uint-rem" (a) `("uint" ,a) ((x `("uint" ,a)) (y `("uint" ,a))) (builder)
  (llvm:build-ret (llvm:build-u-rem builder x y "remainder")))

(defprimpoly "float+" () "float" ((x "float") (y "float")) (builder)
  (llvm:build-ret (llvm:build-f-add builder x y "sum")))

(defprimpoly "float-" () "float" ((x "float") (y "float")) (builder)
  (llvm:build-ret (llvm:build-f-sub builder x y "difference")))

(defprimpoly "float*" () "float" ((x "float") (y "float")) (builder)
  (llvm:build-ret (llvm:build-f-mul builder x y "product")))

(defprimpoly "float/" () "float" ((x "float") (y "float")) (builder)
  (llvm:build-ret (llvm:build-f-div builder x y "quotient")))

(defprimpoly "float-rem" () "float" ((x "float") (y "float")) (builder)
  (llvm:build-ret (llvm:build-f-rem builder x y "remainder")))

(defprimpoly "double+" () "double" ((x "double") (y "double")) (builder)
  (llvm:build-ret (llvm:build-f-add builder x y "sum")))

(defprimpoly "double-" () "double" ((x "double") (y "double")) (builder)
  (llvm:build-ret (llvm:build-f-sub builder x y "difference")))

(defprimpoly "double*" () "double" ((x "double") (y "double")) (builder)
  (llvm:build-ret (llvm:build-f-mul builder x y "product")))

(defprimpoly "double/" () "double" ((x "double") (y "double")) (builder)
  (llvm:build-ret (llvm:build-f-div builder x y "quotient")))

(defprimpoly "double-rem" () "double" ((x "double") (y "double")) (builder)
  (llvm:build-ret (llvm:build-f-rem builder x y "remainder")))

(defmacro defcmps (vars type func &rest ops)
  (cons 'progn
        (loop :for op :in ops :collect
              `(defprimpoly ,(concatenate 'string (string-downcase
                                                   (if (consp type)
                                                       (first type)
                                                       type))
                                          (if (consp op)
                                              (second op)
                                              (string op)))
                   ,vars "bool" ((lhs ,type) (rhs ,type)) (builder)
                 (llvm:build-ret builder (,func builder
                                                ,(if (consp op)
                                                     (first op)
                                                     op)
                                                lhs rhs ""))))))

(defcmps (a) `("int" ,a) llvm:build-i-cmp :> :< := :/= :>= :<=)
(defcmps (a) `("uint" ,a) llvm:build-i-cmp (:unsigned-> ">") (:unsigned-< "<")
  := :/=
  (:unsigned->= ">=") (:unsigned-<= "<="))
(defcmps () "float" llvm:build-f-cmp  :> :< := :/= :>= :<=)
(defcmps () "double" llvm:build-f-cmp  :> :< := :/= :>= :<=)


(defprimpoly "load" (a) a ((pointer `("ptr" ,a))) (builder)
  (llvm:build-ret builder (llvm:build-load builder pointer "value")))
(defprimpoly "store" (a) a ((pointer `("ptr" ,a)) (value a)) (builder)
  (llvm:build-store builder value pointer)
  (llvm:build-ret builder value))

(defprimpoly "vector-elt" (ty size) ty ((vector `("vector" ,ty ,size)) (index '("int" 32))) (builder)
  ;; TODO: Optional bounds checking
  (llvm:build-ret builder (llvm:build-extract-element builder vector index "")))

(defprimpoly "bitcast" (a b) b ((x a)) (builder)
  (llvm:build-ret builder (llvm:build-bit-cast builder x (llvm b) "")))
