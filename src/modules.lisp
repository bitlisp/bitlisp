(in-package #:bitlisp)

(defconstant +module-separator+ #\.)

(defclass module ()
  ((name :initarg :name :reader name)
   (env :initarg :env :reader env)
   (parent :initarg :parent :reader parent)
   (imports :initarg :imports :reader imports)
   (value-exports :initarg :value-exports :reader value-exports)
   (type-exports :initarg :type-exports :reader type-exports)
   (object :initarg :object :initform nil :reader object)))

(defun module-fqn (module)
  (cond
    ;; Necessary for primitives
    ((null module) (concatenate 'string (string +module-separator+) "core"))
    ((null (name module)) "")           ;Identifies root module
    (t (concatenate 'string (module-fqn (parent module))
                    (string +module-separator+) (name (name module))))))

(defun symbol-fqn (module symbol)
  (concatenate 'string (module-fqn module) (string +module-separator+) (name symbol)))

(defun var-fqn (var)
  (concatenate 'string (module-fqn (module (env var)))
               (if (toplevel? (env var))
                   (string +module-separator+)
                   "|")                 ;TODO: Named function envs
               (name (name var))))

(defun poly-instance-fqn (value type)
  (concatenate 'string (var-fqn value) ":" (with-output-to-string (s)
                                             (print type s))))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (princ (module-fqn module) stream)))

(defun make-module (name parent imports &key env type-exports value-exports)
  (let ((menv (or env
                  (make-instance 'environment
                                 :toplevel? t
                                 :parents (when parent
                                            (list (env parent)))))))
    ;; We can't just make these super-envs if we want to respect export lists
    (dolist (import imports)
      (dolist (sym (value-exports import))
        (bind menv :value sym (lookup sym :value (env import))))
      (dolist (sym (type-exports import))
        (bind menv :type sym (lookup sym :type (env import))))
      (dolist (implset (implementations (env import)))
        (push (cdr implset)
              (assoc-value (implementations menv) (car implset)))))
    (prog1-let (mod (make-instance
                     'module
                     :name name
                     :env menv
                     :parent parent
                     :imports imports
                     :value-exports value-exports
                     :type-exports type-exports))
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
