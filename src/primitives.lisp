(in-package #:bitlisp)

(defmacro defctor (name (&rest args) &body builder)
  (with-gensyms (sym)
   `(let ((,sym (make-bl-symbol ,name)))
      (bind *primitives-env* ,sym
            (make-instance 'type-constructor
                           :name ,sym
                           :llvm (lambda ,args ,@builder))))))

(defctor "Int" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "UInt" (bits)
  (check-type bits integer)
  (llvm:int-type bits))
(defctor "Func" (return-type &rest arg-types)
  (llvm:function-type (llvm return-type) (mapcar #'llvm arg-types)))
(defctor "Ptr" (inner-type)
  (llvm:pointer-type (llvm inner-type)))
(defctor "Vector" (inner-type count)
  (check-type count integer)
  (assert (or (eq inner-type (lookup "Bool"))
              (and (typep inner-type 'constructed-type)
                   (member (constructor inner-type)
                           (list (lookup "Int")
                                 (lookup "UInt")
                                 (lookup "Ptr")
                                 (lookup "Float")
                                 (lookup "Double")))))
          (inner-type)
          "Constructing vectors of type ~A is unsupported" inner-type)
  (llvm:vector-type (llvm inner-type) count))

(defun make-ftype (return-type &rest arg-types)
  (make-instance 'constructed-type
                 :constructor (lookup "Func")
                 :args (list* return-type arg-types)))

(defun ftype? (type)
  (or (and (typep type 'constructed-type)
           (eq (lookup "Func") (constructor type)))
      (and (typep type 'universal-type)
           (ftype? (inner-type type)))))

(defun make-ptr (target-type)
  (make-instance 'constructed-type
                 :constructor (lookup "Ptr")
                 :args (list target-type)))

(defmacro defsimpletype (name llvm)
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* ,sym
             (make-instance 'simple-type
                            :name ,sym
                            :llvm ,llvm)))))
(defsimpletype "Unit"   (llvm:void-type))
(defsimpletype "Float"  (llvm:float-type))
(defsimpletype "Double" (llvm:double-type))
(defsimpletype "Bool"   (llvm:int1-type))

(defmacro defint (name width signed)
  (with-gensyms (sym)
    `(let ((,sym (make-bl-symbol ,name)))
       (bind *primitives-env* ,sym
             (make-instance 'constructed-type
                            :constructor ,(if signed
                                              '(lookup "Int")
                                              '(lookup "UInt"))
                            :args '(,width))))))
;;; TODO: Architecture portability
(defint "Word"  32 t)
(defint "UWord" 32 nil)
(defint "Byte"  8  t)
(defint "UByte" 8  nil)

(defparameter *primfun-builders* nil)

(defun build-primfuns (bl-module llvm-module)
  (mapc (lambda (pair)
          (setf (llvm (car pair)) (funcall (cdr pair) bl-module llvm-module)))
        *primfun-builders*))

(defmacro defprimfun (name rtype args (module builder type) &body llvm)
  (with-gensyms (symbol func entry var bl-module)
   `(let* ((,symbol (make-bl-symbol ,name))
           (,type (make-ftype (lookup ,rtype) ,@(mapcar (compose (curry #'list 'lookup) #'second) args)))
           (,var (make-instance 'var
                                :name ,symbol
                                :var-type ,type)))
      (push (cons ,var (lambda (,bl-module ,module)
                         (prog1-let* ((,func (llvm:add-function module
                                                                (symbol-fqn ,bl-module
                                                                            (make-bl-symbol ,name))
                                                                (llvm ,type)))
                                      (,entry (llvm:append-basic-block ,func "entry")))
                           (setf (llvm:function-calling-convention ,func) :fast)
                           (mapc #'(setf llvm:value-name)
                                 ',(mapcar (compose #'string-downcase #'first) args)
                                 (llvm:params ,func))
                           (llvm:with-object (,builder builder)
                             (llvm:position-builder-at-end builder ,entry)
                             (destructuring-bind ,(mapcar #'first args)
                                 (llvm:params ,func)
                               ,@llvm)))))
            *primfun-builders*)
      (bind *primitives-env* ,symbol ,var))))

(defprimfun "word+" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-add builder lhs rhs "sum")))

(defprimfun "word-" "Word" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-sub builder lhs rhs "difference")))

(defprimfun "word>" "Bool" ((lhs "Word") (rhs "Word"))
    (module builder type)
  (llvm:build-ret builder (llvm:build-i-cmp builder :> lhs rhs "greater")))

(defmacro defprimpoly (name vars return-type args (builder) &body instantiator)
  (with-gensyms (module sym type qvars subenv entry func)
    `(let* ((,sym (make-bl-symbol ,name))
            (,qvars (mapcar (lambda (n)
                              (make-instance 'quant-var :name (make-bl-symbol n)))
                            ',(mapcar #'string-downcase vars)))
            (,type (let ((,subenv (make-subenv *primitives-env*)))
                     (mapc (curry #'bind ,subenv)
                           (mapcar #'name ,qvars)
                           ,qvars)
                     (destructuring-bind ,vars (mapcar #'name ,qvars)
                       (make-instance 'universal-type
                                      :variables ,qvars
                                      :inner-type (type-eval (list "Func" ,return-type
                                                                   ,@(mapcar #'second args))
                                                             ,subenv))))))
       (bind *primitives-env* ,sym
             (make-instance 'var
                            :var-type ,type
                            :env *primitives-env*
                            :name ,sym
                            :instantiator
                            (lambda (,module ,@vars)
                              (prog1-let* ((,func (llvm:add-function
                                                   ,module
                                                   (concatenate 'string
                                                                ;; FIXME: Hardcoded module path
                                                                ":core:" ,name
                                                                (format nil "~{!~A~}"
                                                                        (list ,@vars)))
                                                   (llvm (concretify-type ,type ,@vars))))
                                           (,entry (llvm:append-basic-block ,func "entry")))
                                (setf (llvm:function-calling-convention ,func) :fast
                                      (llvm:linkage ,func) :link-once-odr)
                                (mapc #'(setf llvm:value-name)
                                      ',(mapcar (compose #'string-downcase #'first) args)
                                      (llvm:params ,func))
                                (llvm:with-object (,builder builder)
                                  (llvm:position-builder-at-end builder ,entry)
                                  (destructuring-bind ,(mapcar #'first args)
                                      (llvm:params ,func)
                                    ,@instantiator)))))))))

(defprimpoly "load" (a) a ((pointer `("Ptr" ,a))) (builder)
  (declare (ignore a))
  (llvm:build-ret builder (llvm:build-load builder pointer "value")))
(defprimpoly "store" (a) a ((pointer `("Ptr" ,a)) (value a)) (builder)
  (declare (ignore a))
  (llvm:build-store builder value pointer)
  (llvm:build-ret builder value))
