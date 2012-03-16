(in-package #:bitlisp)

(defmacro def-bl-type (name class &rest initargs)
  `(bind *core-env* (make-bl-symbol ,name) (make-instance ',class ,@initargs)))

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
         (make-instance 'value :type ,type)))

(defprimfun "word+" (make-instance 'constructed-type
                                   ;; TODO: Real function constructor
                                   :constructor :func
                                   :args (let ((ty (lookup (sym "Word"))))
                                           (list ty ty ty))))
