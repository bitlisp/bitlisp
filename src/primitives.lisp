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

(defun add-prim (name scheme llvm)
  (bind *primitives-env* :value name
        (make-instance (if (vars scheme)
                           'prim-poly-value
                           'value)
                       :name name
                       :value-type scheme
                       :env *primitives-env*
                       :llvm llvm)))

(defmacro def-prim (name type (instance-type module) &body body)
  `(add-prim (make-bl-symbol ,name) (multiple-value-bind (ty preds)
                                        (type-eval ',type)
                                      (quantify (free-vars ty) preds ty))
             (lambda (,instance-type ,module) ,@body)))

(defmacro def-binop (name tyvars arg-type return-type builder)
  `(def-prim ,name ("forall" ,tyvars ()
                             ("func" ("*" ,arg-type ,arg-type) ,return-type))
       (type module)
     (with-func (func builder module type)
       (destructuring-bind (left right) (llvm:params func)
         (llvm:build-ret builder (,builder builder left right ""))))))

(def-binop "int+" ("a") ("int" "a") ("int" "a") llvm:build-add)
(def-binop "int-" ("a") ("int" "a") ("int" "a") llvm:build-sub)
(def-binop "int/" ("a") ("int" "a") ("int" "a") llvm:build-s-div)
(def-binop "int-rem" ("a") ("int" "a") ("int" "a") llvm:build-s-rem)

(def-binop "uint+" ("a") ("uint" "a") ("uint" "a") llvm:build-add)
(def-binop "uint-" ("a") ("uint" "a") ("uint" "a") llvm:build-sub)
(def-binop "uint/" ("a") ("uint" "a") ("uint" "a") llvm:build-u-div)
(def-binop "uint-rem" ("a") ("uint" "a") ("uint" "a") llvm:build-u-rem)

(def-binop "float+" () "float" "float" llvm:build-f-add)
(def-binop "float-" () "float" "float" llvm:build-f-sub)
(def-binop "float/" () "float" "float" llvm:build-f-div)
(def-binop "float-rem" () "float" "float" llvm:build-f-rem)

(def-binop "double+" () "double" "double" llvm:build-f-add)
(def-binop "double-" () "double" "double" llvm:build-f-sub)
(def-binop "double/" () "double" "double" llvm:build-f-div)
(def-binop "double-rem" () "double" "double" llvm:build-f-rem)

(defmacro defcmps (vars type func &rest ops)
  (cons 'progn
        (loop :for op :in ops :collect
              `(def-binop ,(concatenate 'string (string-downcase
                                                   (if (consp type)
                                                       (first type)
                                                       type))
                                        (if (consp op)
                                            (second op)
                                            (string op)))
                   ,vars ,type "bool"
                 (lambda (builder left right name)
                   (,func builder ,(if (consp op)
                                       (first op)
                                       op)
                          left right name))))))

(defcmps ("a") ("int" "a") llvm:build-i-cmp :> :< := :/= :>= :<=)
(defcmps ("a") ("uint" "a") llvm:build-i-cmp (:unsigned-> ">") (:unsigned-< "<")
  := :/=
  (:unsigned->= ">=") (:unsigned-<= "<="))
(defcmps () "float" llvm:build-f-cmp  :> :< := :/= :>= :<=)
(defcmps () "double" llvm:build-f-cmp  :> :< := :/= :>= :<=)

(def-prim "load" ("forall" ("a") () ("func" ("ptr" "a") "a")) (type module)
  (with-func (func builder module type)
    (llvm:build-ret builder (llvm:build-load builder (first (llvm:params func)) ""))))
(def-prim "store" ("forall" ("a") () ("func" ("*" ("ptr" "a") "a") "a")) (type module)
  (with-func (func builder module type)
    (destructuring-bind (pointer value) (llvm:params func)
      (llvm:build-store builder value pointer)
      (llvm:build-ret builder value))))

;;; TODO: s/(int 32)/word/
(def-prim "vector-elt" ("forall" ("ty" "size") () ("func" ("*" ("vector" "ty" "size") ("int" 32)) "ty")) (type module)
  (with-func (func builder module type)
    (destructuring-bind (vector index) (llvm:params func)
      (llvm:build-ret builder (llvm:build-extract-element builder vector index "")))))

(def-prim "bitcast" ("forall" ("a" "b") () ("func" "a" "b")) (type module)
  (with-func (func builder module type)
    (llvm:build-ret builder (llvm:build-bit-cast builder (first (llvm:params func)) (llvm (second (args type))) ""))))
