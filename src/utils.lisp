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
