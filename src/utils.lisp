(in-package #:bitlisp)

(defmacro prog1-let (bindings &body body)
  "Executes body with BINDINGS bound, returning the initial value of the first binding. BINDINGS may be either a single name-value pair or a let-style binding list."
  (if (symbolp (first bindings))
      `(let (,bindings)
         (prog1 ,(first bindings)
           ,@body))
      `(let ,bindings
         (prog1 ,(first (first bindings))
           ,@body))))

(defmacro prog1-let* (bindings &body body)
  "Executes body with BINDINGS bound, returning the initial value of the first binding. BINDINGS may be either a single name-value pair or a let*-style binding list."
  (if (symbolp (first bindings))
      `(let (,bindings)
         (prog1 ,(first bindings)
           ,@body))
      `(let* ,bindings
         (prog1 ,(first (first bindings))
           ,@body))))

(defun random-string (size)
  (write-to-string (random (expt 36 size)) :base 36))

(defmacro with-tmp-file (var &body body)
  (with-gensyms (handle)
    `(let ((,handle nil)
           (,var))
       (loop :until ,handle :do
         (setf ,var (concatenate 'string "/tmp/" (random-string 8))
               ,handle (open ,var :direction :output :if-exists nil)))
       (close ,handle)
       (unwind-protect (progn ,@body)
         (delete-file ,var)))))
