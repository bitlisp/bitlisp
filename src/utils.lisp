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
