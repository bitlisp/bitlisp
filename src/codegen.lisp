(in-package #:bitlisp)

(defun compile-full (sexps &key (outpath "./bitlisp.s") (speed 3) (assemble t))
  (let ((root-module (make-root)))
    (with-tmp-file (bc "bitlisp-")
      (compile-unit sexps root-module
                    :outpath bc
                    :main-unit? t)
      (with-tmp-file (core "bitlisp-core-")
        (llvm:with-object (core-module module ":core")
          (build-primfuns (lookup "core" (env root-module)) core-module)
          (llvm:write-bitcode-to-file core-module core))
        (ccl:run-program "llvm-link" (list "-o" bc core bc)
                         :output *standard-output* :error *error-output*))
      (let ((opt (format nil "-O~D" speed)))
        (ccl:run-program "opt" (list opt "-o" (if assemble bc outpath) bc)
                        :output *standard-output* :error *error-output*)
        (when assemble
          (ccl:run-program "llc" (list opt "-o" outpath bc)
                           :output *standard-output* :error *error-output*))))))

(defun compile-unit (sexps base-module &key (outpath "./bitlisp.bc") main-unit?)
  (multiple-value-bind (module-form predicates unit-module)
      (build-types (env base-module) (first sexps))
    (assert (and (listp (form-code module-form))
                 (eq (first (form-code module-form)) (lookup "module")))
            ()
            "Compilation units must begin with a module declaration! (got ~A)"
            module-form)
    (llvm:with-object (llvm-module module (module-fqn unit-module))
      (codegen llvm-module nil module-form)
      ;; nil IR builder argument here indicates toplevel
      ;; TODO: Toplevel forms should be permissible and execute before main
      (mapc (compose (curry #'codegen llvm-module nil)
                     (curry #'build-types (env unit-module)))
            (rest sexps))
      (dolist (export (exports unit-module))
        (setf (llvm:linkage (llvm (lookup export (env unit-module)))) :external))
      (when main-unit?
        (multiple-value-bind (var exists?)
            (lookup "main" (env unit-module))
          (assert exists? () "No main bound in ~A" base-module)
          (compile-main llvm-module var)))
      (llvm:write-bitcode-to-file llvm-module outpath))))

(defun compile-main (llvm-module internal-main)
  (let ((ftype (type-eval '("Func" "Word" "Word" ("Ptr" ("Ptr" "Byte"))))))
   (assert (bl-type= (var-type internal-main) ftype)
           ()
           "main is of inappropriate type ~A (should be ~A)"
           (var-type internal-main) ftype)
    (llvm:with-objects ((builder builder))
      (let* ((main (llvm:add-function llvm-module "main" (llvm ftype)))
             (entry (llvm:append-basic-block main "entry"))
             (params (llvm:params main)))
        (setf (llvm:value-name (first params)) "argc"
              (llvm:value-name (second params)) "argv")
        (llvm:position-builder-at-end builder entry)
        (let ((call (llvm:build-call builder (llvm internal-main) (llvm:params main)
                                     "")))
          (setf (llvm:instruction-calling-convention call) :fast)
          (llvm:build-ret builder call))))))

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
      (var (if (vars (var-type code))
               ;; TODO: Cache polymorphic instantiations
               (funcall (llvm code) llvm-module type)
               (llvm code)))
      (list
       (destructuring-bind (op &rest args) code
         (etypecase op
           (special-op (apply (special-op-codegen op)
                              llvm-module builder type (print args)))
           (form (let ((argvals (mapcar (curry #'codegen llvm-module builder)
                                        args)))
                   (prog1-let (call (llvm:build-call builder (codegen llvm-module builder op)
                                                     argvals ""))
                     (setf (llvm:instruction-calling-convention call) :fast))))))))))
