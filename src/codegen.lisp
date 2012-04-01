(in-package #:bitlisp)

(defun compile-full (sexps &optional (outpath "./bitlisp.s"))
  (let ((root-module (make-root)))
    (with-tmp-file (bc "bitlisp-")
      (compile-unit sexps :base-module root-module
                          :outpath bc)
      (with-tmp-file (core "bitlisp-core-")
        (llvm:with-object (core-module module ":core")
          (build-primfuns (lookup "core" (env root-module)) core-module)
          (llvm:write-bitcode-to-file core-module core))
        (ccl:run-program "llvm-link" (list "-o" bc core bc)
                         :output *standard-output* :error *error-output*))
      (ccl:run-program "opt" (list "-O3" "-o" bc bc)
                       :output *standard-output* :error *error-output*)
      (ccl:run-program "llc" (list "-O3" "-o" outpath bc)
                       :output *standard-output* :error *error-output*))))

(defun compile-unit (sexps &key base-module (outpath "./bitlisp.bc"))
  (multiple-value-bind (module-form next-module) (build-types base-module (first sexps))
    (assert (and (listp (form-code module-form))
                 (eq (first (form-code module-form)) (lookup "module")))
            ()
            "Compilation units must begin with a module declaration! (got ~A)"
            module-form)
    (llvm:with-object (llvm-module module (module-fqn next-module))
      (codegen llvm-module nil module-form)
      ;; nil IR builder argument here indicates toplevel
      ;; TODO: Toplevel forms should be permissible and execute at startup
      (mapc (compose (curry #'codegen llvm-module nil)
                     (curry #'build-types next-module))
            (rest sexps))
      (llvm:write-bitcode-to-file llvm-module outpath))))

(defun codegen (llvm-module builder form)
  (destructuring-bind (type . code) form
    (etypecase code
      (string (let* ((value (llvm:const-string code nil))
                     (global (llvm:add-global llvm-module (llvm:type-of value) "literal-str")))
                (setf (llvm:initializer global) value
                      (llvm:linkage global) :private
                      (llvm:global-constant-p global) t)
                (llvm:const-in-bounds-gep global
                                          (mapcar (curry #'llvm:const-int (llvm:int32-type))
                                                  '(0 0)))))
      (integer (llvm:const-int (llvm type) code))
      (real (llvm:const-real (llvm type) code))
      (var (llvm code))
      (list
       (destructuring-bind (op &rest args) code
         (etypecase op
           (special-op (apply (special-op-codegen op)
                              llvm-module builder type args))
           (form (let ((func (codegen llvm-module builder op))
                       (argvals (mapcar (curry #'codegen llvm-module builder)
                                        args)))
                   (mapc #'llvm:dump-value argvals)
                   (prog1-let (call (llvm:build-call builder func
                                                     argvals
                                                     "result"))
                     (setf (llvm:instruction-calling-convention call) :fast))))))))))
