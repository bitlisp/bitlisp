(in-package #:bitlisp)

(defclass module ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (parent :initarg :parent :reader parent)
   (imports :initarg :imports :reader imports)
   (value-exports :initarg :value-exports :reader value-exports)
   (type-exports :initarg :type-exports :reader type-exports)
   (interface-exports :initarg :interface-exports :reader interface-exports)
   (object :initarg :object :initform nil :reader object)))

(defun module-fqn (module)
  (cond
    ((null module) ":core")             ;Necessary for primitives
    ((null (name module)) "")           ;Identifies root module
    (t (concatenate 'string (module-fqn (parent module))
                    ":" (name (name module))))))

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

(defun make-module (name parent imports &key env interface-exports type-exports value-exports)
  (let ((menv (or env
                  (make-instance 'environment
                                 :toplevel? t
                                 :parents (when parent
                                            (list (env parent)))))))
    (dolist (import imports)
      (dolist (sym (value-exports import))
        ;; TODO: Import non-values
        (bind menv :value sym (lookup sym :value (env import))))
      (dolist (sym (interface-exports import))
        ;; TODO: Import non-values
        (bind menv :interface sym (lookup sym :value (env import))))
      (dolist (sym (type-exports import))
        ;; TODO: Import non-values
        (bind menv :type sym (lookup sym :value (env import)))))
    (prog1-let (mod (make-instance
                     'module
                     :name name
                     :env menv
                     :parent parent
                     :imports imports
                     :value-exports value-exports
                     :type-exports type-exports
                     :interface-exports interface-exports))
      (setf (module menv) mod)
      (bind (env parent) :value name mod))))

(defun make-root ()
  (let ((root-env (make-instance 'environment :toplevel? t))
        primitive-values primitive-types)
   (prog1-let (root (make-instance 'module
                                   :name nil
                                   :env root-env))
     (setf (module root-env) root)
     (maphash (lambda (sym var)
                (declare (ignore var))
                (push sym primitive-values))
              (value-bindings *primitives-env*))
     (maphash (lambda (sym var)
                (declare (ignore var))
                (push sym primitive-types))
              (type-bindings *primitives-env*))
     (make-module (make-bl-symbol "core") root nil
                  :env *primitives-env*
                  :value-exports primitive-values
                  :type-exports primitive-types)
     (bind (env root) :value (make-bl-symbol "module") (lookup "module")))))
