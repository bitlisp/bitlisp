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
(defctor "Bool"   () (llvm:int1-type))
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
  (assert (or (eq inner-type (lookup "Bool" :type))
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

(defmacro defint (name width signed)
  ;; TODO: Don't just alias.
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* :type ,sym
             (tyapply ,(if signed
                           '(lookup "int" :type)
                           '(lookup "uint" :type))
                      ,width)))))
;;; TODO: Architecture portability for word type
(defint "word"      32 t)
(defint "uword"     32 nil)
(defint "byte"      8  t)
(defint "ubyte"     8  nil)
(defint "codepoint" 32 nil)

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
                            (prog1-let (,func (llvm:add-function module
                                                                 (symbol-fqn ,bl-module
                                                                             (make-bl-symbol ,name))
                                                                 (llvm ,type)))
                              (setf (llvm:function-calling-convention ,func) :fast)
                              (mapc #'(setf llvm:value-name)
                                    ',(mapcar (compose #'string-downcase #'first) args)
                                    (llvm:params ,func))
                              (llvm:with-object (,builder builder)
                                (llvm:position-builder-at-end builder
                                                              (llvm:append-basic-block ,func "entry"))
                                (destructuring-bind ,(mapcar #'first args)
                                    (llvm:params ,func)
                                  ,@llvm)))))
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
              (lambda (,module ,inst-type)
                (prog1-let (,func (llvm:add-function ,module (concatenate 'string (module-fqn nil) ":" ,name "!" (princ-to-string ,type)) (llvm ,inst-type)))
                  (llvm:with-object (,builder builder)
                    (llvm:position-builder-at-end ,builder (llvm:append-basic-block ,func "entry"))
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

(defmacro deficmps (types &rest ops)
  (cons 'progn
        (loop :for type :in types :nconc
          (loop :for op :in ops :collect
                `(defprimpoly ,(concatenate 'string (string-downcase type) (string op))
                     (a) "Bool" ((lhs `(,,type ,a)) (rhs `(,,type ,a))) (builder)
                   (llvm:build-ret builder (llvm:build-i-cmp builder ,op lhs rhs "")))))))

(deficmps ("int" "uint") :> :< := :/= :>= :<=)

(defprimpoly "load" (a) a ((pointer `("ptr" ,a))) (builder)
  (llvm:build-ret builder (llvm:build-load builder pointer "value")))
(defprimpoly "store" (a) a ((pointer `("ptr" ,a)) (value a)) (builder)
  (llvm:build-store builder value pointer)
  (llvm:build-ret builder value))
