(in-package #:bitlisp)

(defun compile-full (forms &optional (outpath "./bitlisp.s"))
  (with-tmp-file bc
    (compile-module forms bc)
    (ccl:run-program "opt" (list "-O3" "-o" bc bc))
    (ccl:run-program "llc" (list "-O3" "-o" outpath bc))))

(defun compile-module (forms &optional (outpath "./bitlisp.bc"))
  (llvm:with-module (module "bitlisp")
    ;; TODO: Compile these once per compiler instance, or even load them from LLVM IR.
    (build-primfuns module)
    ;; nil builder indicates top level
    (mapc (curry 'codegen module nil) forms)
    (llvm:write-bitcode-to-file module outpath)))

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
