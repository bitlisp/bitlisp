(in-package #:bitlisp)

(defclass bl-symbol ()
  ((name :initarg :name :reader name)
   (id :initarg :id :reader id)))

(defmethod print-object ((symbol bl-symbol) stream)
  (princ (name symbol) stream))

(defvar *symbol-name-table* (make-hash-table :test 'equal))

(defvar *symbol-id-table* (make-array '(128)
                                      :element-type 'bl-symbol
                                      :adjustable t
                                      :fill-pointer 0))

(defun make-bl-symbol (name)
  (or (gethash name *symbol-name-table*)
      (let ((symbol (make-instance 'bl-symbol
                                   :name name
                                   :id (fill-pointer *symbol-id-table*))))
        (vector-push-extend symbol *symbol-id-table*)
        (setf (gethash (name symbol) *symbol-name-table*) symbol))))

(defun sym (name) (make-bl-symbol name))

(defun ensure-bl-sym (thing)
  (etypecase thing
    (symbol (ensure-bl-sym (string thing)))
    (string (make-bl-symbol thing))
    (bl-symbol thing)))
