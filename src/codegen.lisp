(in-package #:bitlisp)

;; 01:26:54 < nicholas> Ralith: ah i see. opt -internalize changes the
;;                      linkage type, iff you tell it what symbols are
;;                      exported or your combined .bc has a function
;;                      named main.
;; 01:27:09 < nicholas> which is the first thing that -std-link-opts
;;                      does internally
;; 01:27:31 < nicholas> you can pass it the list of functions via
;;                      -internalize-public-api-{file,list}=...
(defun compile-full (sexps &key (outpath "./bitlisp.s") (speed 3) (assemble t))
  (let ((root-module (make-root)))
    (with-tmp-file (bc "bitlisp-")
      (compile-unit sexps root-module
                    :outpath bc
                    :main-unit? t)
      (with-tmp-file (core "bitlisp-core-")
        (llvm:with-object (core-module module ":core")
          (build-primfuns (lookup "core" :value (env root-module)) core-module)
          (llvm:write-bitcode-to-file core-module core))
        (ccl:run-program "llvm-link" (list "-o" bc core bc)
                         :output *standard-output* :error *error-output*))
      (let ((opt (format nil "-O~D" speed)))
        (ccl:run-program "opt" (list opt "-o" (if assemble bc outpath) bc
                                     "-std-link-opts")
                         :output *standard-output* :error *error-output*)
        (when assemble
          (ccl:run-program "llc" (list opt "-o" outpath bc)
                           :output *standard-output* :error *error-output*))))))

(defun compile-unit (sexps base-module &key (outpath "./bitlisp.bc") main-unit?)
  (multiple-value-bind (module-decl module) (resolve (first sexps) (env base-module))
    (assert module ()
            "Compilation units must begin with a module declaration!")
    (llvm:with-object (llvm-module module (module-fqn module))
      (codegen llvm-module nil (infer-expr module-decl))
      ;; nil IR builder argument here indicates toplevel
      ;; TODO: Toplevel forms should be permissible and execute before main
      (mapc (compose (curry #'codegen llvm-module nil)
                     #'infer-expr
                     (rcurry #'resolve (env module)))
            (rest sexps))
      (dolist (export (value-exports module))
        (setf (llvm:linkage (llvm (lookup export :value (env module)))) :external))
      (when main-unit?
        (multiple-value-bind (var exists?)
            (lookup "main" :value (env module))
          (assert exists? () "No main bound in ~A" base-module)
          (compile-main llvm-module var)))
      (llvm:write-bitcode-to-file llvm-module outpath))))

(defun compile-main (llvm-module internal-main)
  (let ((ftype (type-eval '("func" ("*" "word" ("ptr" ("ptr" "byte")))
                            "word"))))
   (assert (bl-type= (value-type internal-main) ftype)
           ()
           "main is of inappropriate type ~A (should be ~A)"
           (value-type internal-main) ftype)
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
  (if (null form) nil
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
          (value (if (vars (value-type code))
                     (poly-value-instance code type llvm-module)
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
                         (setf (llvm:instruction-calling-convention call) :fast)))))))))))

(defun poly-value-instance (poly-value monotype llvm-module)
  (or (assoc-value (instances poly-value) monotype :test #'bl-type=)
      (setf (assoc-value (instances poly-value) monotype :test #'bl-type=)
            (if (typep poly-value 'prim-poly-value)
                (funcall (llvm poly-value) monotype llvm-module)
                (codegen llvm-module nil
                         (specialize-form (form poly-value) monotype))))))
