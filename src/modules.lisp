(in-package #:bitlisp)

(defclass module ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (parent :initarg :parent :reader parent)
   (imports :initarg :imports :reader imports)
   (exports :initarg :exports :reader exports)))

(defun module-fqn (module)
  (if (null (name module))              ;Identifies root module
      ""
      (concatenate 'string (module-fqn (parent module))
                   ":" (name (name module)))))

(defun symbol-fqn (module symbol)
  (concatenate 'string (module-fqn module) ":" (name symbol)))

(defun var-fqn (var)
  (concatenate 'string (module-fqn (module (env var)))
               (if (toplevel? (env var))
                   ":"
                   "..")
               (name (name var))))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (princ (module-fqn module) stream)))

(defun make-module (name parent imports exports &optional env)
  (let ((menv (or env
                  (make-instance 'environment
                                 :toplevel? t
                                 :parents (if parent
                                              (cons (env parent)
                                                    (mapcar #'env imports))
                                              (mapcar #'env imports))))))
    (prog1-let (mod (make-instance
                     'module
                     :name name
                     :env menv
                     :parent parent
                     :imports imports
                     :exports exports))
      (setf (module menv) mod)
      (bind (env parent) name mod))))

(defun make-root ()
  (let ((root-env (make-instance 'environment :toplevel? t)))
   (prog1-let (root (make-instance 'module
                                   :name nil
                                   :env root-env))
     (setf (module root-env) root)
     (make-module (make-bl-symbol "core") root nil nil *primitives-env*)
     (bind (env root) (make-bl-symbol "module") (lookup "module" *primitives-env*)))))