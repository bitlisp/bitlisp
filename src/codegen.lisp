(in-package #:bitlisp)

(defun compile-module (forms)
  (llvm:with-module (module "bitlisp")
    ;; TODO: Compile these once per compiler instance, or even load them from LLVM IR.
    (build-primfuns module)
    ;; nil builder indicates top level
    (mapc (curry 'codegen module nil) forms)
    (llvm:dump-module module)))

(defun codegen (module builder form)
  (destructuring-bind (type . code) form
    (etypecase code
      (integer (llvm:const-int (llvm type) code))
      (real (llvm:const-real (llvm type) code))
      (var (llvm code))
      (list
       (destructuring-bind (op &rest args) code
         (etypecase op
           (special-op (apply (special-op-codegen op)
                              module builder type args))
           (form (llvm:build-call builder (codegen module builder op)
                                  (map 'vector (curry #'codegen module builder) args)
                                  "result"))))))))
