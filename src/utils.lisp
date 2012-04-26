(in-package #:bitlisp)

(defun random-string (size)
  (write-to-string (random (expt 36 size)) :base 36))

(defmacro with-tmp-file ((var &optional (prefix "")) &body body)
  (with-gensyms (handle)
    `(let ((,handle nil)
           (,var))
       (loop :until ,handle :do
         (setf ,var (concatenate 'string "/tmp/" ,prefix (random-string 8))
               ,handle (open ,var :direction :output
                                  :if-exists nil)))
       (close ,handle)
       (unwind-protect (progn ,@body)
         (delete-file ,var)))))

(defun partition (predicate list)
  (loop :for item :in list
        :with matches := nil
        :with failures := nil
        :do (if (funcall predicate item)
                (push item matches)
                (push item failures))
        :finally (return (values matches failures))))

(defun build-func (module type &key (name "function") arg-names (linkage :internal) (calling-convention :fast) attributes)
  (let ((func (llvm:add-function module name (llvm type))))
    (setf (llvm:function-calling-convention func) calling-convention)
    (setf (llvm:linkage func) linkage)
    (when attributes
     (apply #'llvm:add-function-attributes func attributes))
    (mapc #'(setf llvm:value-name) arg-names (llvm:params func))))

(defmacro with-func ((func-var builder-var module type &rest keys &key (name "function") arg-names (linkage :internal) (calling-convention :fast) attributes) &body body)
  "Executes BODY with FUNC-VAR bound to a new function of type TYPE and BUILDER-VAR bound to a new instruction builder positioned at the end of its entry block. Returns the function handle."
  (declare (ignore name arg-names linkage calling-convention attributes))
  `(llvm:with-object (,builder-var builder)
     (let ((,func-var (build-func ,module ,type ,@keys)))
       (llvm:position-builder-at-end ,builder-var
                                     (llvm:append-basic-block ,func-var "entry"))
       ,@body
       ,func-var)))
