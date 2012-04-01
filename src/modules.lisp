(in-package #:bitlisp)

(defclass module ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (parent :initarg :parent :reader parent)
   (imports :initarg :imports :reader imports)
   (exports :initarg :exports :accessor exports)))

(defun module-fqn (module)
  (if (eq module *root-module*)
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
    (if (eq module *root-module*)
        (princ "root" stream)
        (princ (module-fqn module) stream))))

(defun make-module (name parent imports exports)
  (let ((menv (make-instance 'environment
                             :toplevel? t
                             :parents (if parent
                                          (cons (env parent)
                                                (mapcar #'env imports))
                                          (mapcar #'env imports)))))
    (prog1-let (mod (make-instance
                     'module
                     :name name
                     :env menv
                     :parent parent
                     :imports imports
                     :exports exports))
      (setf (module menv) mod)
      (when parent
        (bind (env parent) name mod)))))

(defvar *root-module* (make-module nil nil nil nil))
(defvar *core-module* (make-module (make-bl-symbol "core") *root-module* nil nil))
(defvar *scratch-module* (make-module (make-bl-symbol "scratch") *root-module* (list *core-module*) nil))
