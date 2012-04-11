(in-package #:bitlisp)

(defmacro defctor (name (&rest args) &body builder)
  (with-gensyms (sym)
   `(let ((,sym (make-bl-symbol ,name)))
      (bind *primitives-env* ,sym
            (make-instance 'tycon
                           :name ,sym
                           :kind ,(1+ (length args))
                           :llvm (lambda ,args ,@builder))))))

(defctor "Unit"   () (llvm:void-type))
(defctor "Bool"   () (llvm:int1-type))
(defctor "Float"  () (llvm:float-type))
(defctor "Double" () (llvm:double-type))
(defctor "Int" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "UInt" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "Func" (arg-type return-type)
  (llvm:function-type (llvm return-type)
                      (cond
                        ((prodtype? arg-type)
                         (mapcar #'llvm (flatten-product arg-type)))
                        ((eq arg-type (lookup "Unit")) nil)
                        (t (llvm arg-type)))))
(defctor "Ptr" (inner-type)
  (llvm:pointer-type (llvm inner-type)))
(defctor "Vector" (inner-type count)
  (check-type count integer)
  (assert (or (eq inner-type (lookup "Bool"))
              (and (typep inner-type 'tyapp)
                   (member (operator inner-type)
                           (list (lookup "Int")
                                 (lookup "UInt")
                                 (lookup "Ptr")
                                 (lookup "Float")
                                 (lookup "Double")))))
          (inner-type)
          "Constructing vectors of type ~A is unsupported" inner-type)
  (llvm:vector-type (llvm inner-type) count))
(defctor "*" (l r)
  (llvm:struct-type
   (mapcar #'llvm (nconc (flatten-product l) (flatten-product r)))
   nil))

(defun ftype? (type)
  (or (and (typep type 'tyapp)
           (eq (lookup "Func") (operator type)))
      (and (typep type 'scheme)
           (ftype? (inner-type type)))))

(defun prodtype? (type)
  (or (and (typep type 'tyapp)
           (eq (lookup "*") (operator type)))
      (and (typep type 'scheme)
           (prodtype? (inner-type type)))))

(defun flatten-product (product)
  (if (prodtype? product)
      (destructuring-bind (l r) (args product)
        (nconc (flatten-product l) (flatten-product r)))
      (list product)))

(defmacro defint (name width signed)
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* ,sym
             (make-instance 'tyapp
                            :operator ,(if signed
                                           '(lookup "Int")
                                           '(lookup "UInt"))
                            :args '(,width))))))
;;; TODO: Architecture portability for word type
(defint "Word"      32 t)
(defint "UWord"     32 nil)
(defint "Byte"      8  t)
(defint "UByte"     8  nil)
(defint "Codepoint" 32 nil)

(defparameter *primfun-builders* nil)

(defun build-primfuns (bl-module llvm-module)
  (mapc (lambda (pair)
          (setf (llvm (car pair)) (funcall (cdr pair) bl-module llvm-module)))
        *primfun-builders*))

(defmacro defprimfun (name rtype args (module builder type) &body llvm)
  (with-gensyms (symbol func var bl-module)
   `(let* ((,symbol (make-bl-symbol ,name))
           (,type (make-ftype (make-prodty ,@(mapcar (compose (curry #'list 'lookup) #'second) args))
                              (lookup ,rtype)))
           (,var (make-instance 'var
                                :name ,symbol
                                :var-type ,type)))
      (push (cons ,var (lambda (,bl-module ,module)
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
      (bind *primitives-env* ,symbol ,var))))

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
                     (destructuring-bind ,vars ,qvars
                       (make-instance
                        'scheme
                        :vars ,qvars
                        :inner-type
                        (make-instance
                         'tyqual
                         :context nil
                         :head
                         (type-eval (list "Func" (make-prodty ,@(mapcar #'second args))
                                          ,return-type)
                                    ,subenv)))))))
       (bind *primitives-env* ,sym
             (make-instance
              'prim-poly-var
              :var-type ,type
              :env *primitives-env*
              :name ,sym
              :llvm
              (lambda (,module ,inst-type)
                (prog1-let (,func (llvm:add-function ,module (concatenate 'string ":core:" ,name "!" (princ-to-string ,type)) (llvm ,inst-type)))
                  (llvm:with-object (,builder builder)
                    (llvm:position-builder-at-end ,builder (llvm:append-basic-block ,func "entry"))
                    (destructuring-bind ,(mapcar #'first args) (llvm:params ,func)
                      ,@instantiator)))))))))

(defprimpoly "int+" (a) `("Int" ,a) ((x `("Int" ,a)) (y `("Int" ,a))) (builder)
  (llvm:build-ret (llvm:build-add builder x y "sum")))

(defprimpoly "int-" (a) `("Int" ,a) ((x `("Int" ,a)) (y `("Int" ,a))) (builder)
  (llvm:build-ret (llvm:build-sub builder x y "difference")))

(defprimpoly "int*" (a) `("Int" ,a) ((x `("Int" ,a)) (y `("Int" ,a))) (builder)
  (llvm:build-ret (llvm:build-mul builder x y "product")))

(defprimpoly "int/" (a) `("Int" ,a) ((x `("Int" ,a)) (y `("Int" ,a))) (builder)
  (llvm:build-ret (llvm:build-s-div builder x y "quotient")))

(defprimpoly "int-rem" (a) `("Int" ,a) ((x `("Int" ,a)) (y `("Int" ,a))) (builder)
  (llvm:build-ret (llvm:build-s-rem builder x y "remainder")))

(defprimpoly "uint+" (a) `("UInt" ,a) ((x `("UInt" ,a)) (y `("UInt" ,a))) (builder)
  (llvm:build-ret (llvm:build-add builder x y "sum")))

(defprimpoly "uint-" (a) `("UInt" ,a) ((x `("UInt" ,a)) (y `("UInt" ,a))) (builder)
  (llvm:build-ret (llvm:build-sub builder x y "difference")))

(defprimpoly "uint*" (a) `("UInt" ,a) ((x `("UInt" ,a)) (y `("UInt" ,a))) (builder)
  (llvm:build-ret (llvm:build-mul builder x y "product")))

(defprimpoly "uint/" (a) `("UInt" ,a) ((x `("UInt" ,a)) (y `("UInt" ,a))) (builder)
  (llvm:build-ret (llvm:build-u-div builder x y "quotient")))

(defprimpoly "uint-rem" (a) `("UInt" ,a) ((x `("UInt" ,a)) (y `("UInt" ,a))) (builder)
  (llvm:build-ret (llvm:build-u-rem builder x y "remainder")))

(defmacro deficmps (types &rest ops)
  (cons 'progn
        (loop :for type :in types :nconc
          (loop :for op :in ops :collect
                `(defprimpoly ,(concatenate 'string (string-downcase type) (string op))
                     (a) "Bool" ((lhs `(,,type ,a)) (rhs `(,,type ,a))) (builder)
                   (llvm:build-ret builder (llvm:build-i-cmp builder ,op lhs rhs "")))))))

(deficmps ("Int" "UInt") :> :< := :/= :>= :<=)

(defprimpoly "load" (a) a ((pointer `("Ptr" ,a))) (builder)
  (llvm:build-ret builder (llvm:build-load builder pointer "value")))
(defprimpoly "store" (a) a ((pointer `("Ptr" ,a)) (value a)) (builder)
  (llvm:build-store builder value pointer)
  (llvm:build-ret builder value))
