(in-package #:bitlisp)

(defun compile-full (sexps &optional (outpath "./bitlisp.s"))
  (let ((root-module (make-root)))
    (with-tmp-file bc
      (compile-unit sexps :base-module root-module
                          :outpath bc)
      (with-tmp-file core
        (llvm:with-object (core-module module ":core")
          (build-primfuns (lookup "core" (env root-module)) core-module)
          (llvm:write-bitcode-to-file core-module core))
        (ccl:run-program "llvm-link" (list "-o" bc core bc)))
      (ccl:run-program "opt" (list "-O3" "-o" bc bc))
      (ccl:run-program "llc" (list "-O3" "-o" outpath bc)))))

(defun compile-unit (sexps &key base-module (outpath "./bitlisp.bc"))
  (multiple-value-bind (module-form next-module) (build-types base-module (first sexps))
    (assert (and (listp (form-code module-form))
                 (eq (first (form-code module-form)) (lookup "module")))
            ()
            "Compilation units must begin with a module declaration! (got ~A)"
            module-form)
    (llvm:with-object (llvm-module module (module-fqn next-module))
      (codegen llvm-module nil module-form)
      (compile-in-module (mapcar (curry #'build-types next-module)
                             (rest sexps))
                      llvm-module
                      outpath))))

(defun compile-in-module (forms llvm-module &optional (outpath "./bitlisp.bc"))
  ;; nil builder indicates top level
  (mapc (curry 'codegen llvm-module nil) forms)
  (llvm:write-bitcode-to-file llvm-module outpath))

(defun codegen (llvm-module builder form)
  (destructuring-bind (type . code) form
    (etypecase code
      (integer (llvm:const-int (llvm type) code))
      (real (llvm:const-real (llvm type) code))
      (var (llvm code))
      (list
       (destructuring-bind (op &rest args) code
         (etypecase op
           (special-op (apply (special-op-codegen op)
                              llvm-module builder type args))
           (form (prog1-let (call (llvm:build-call builder (codegen llvm-module builder op)
                                                   (map 'vector
                                                        (curry #'codegen llvm-module builder)
                                                        args)
                                                   "result"))
                   (setf (llvm:instruction-calling-convention call) :fast)))))))))
