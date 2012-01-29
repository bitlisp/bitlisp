(in-package #:bitlisp)

(defgeneric llvm (object))

(defclass value ()
  ((contents :initarg :contents :reader contents)
   (bl-type :initarg :bl-type :reader bl-type)
   (llvm :initarg :llvm :reader llvm)))

(defclass place ()
  ((bl-type :initarg :bl-type :reader bl-type)
   (llvm :initarg :llvm :accessor llvm)))

(defclass special-op ()
  ((name :initarg :name :reader name)
   (resolver :initarg :resolver :reader resolver)
   (typer :initarg :typer :reader typer)
   (constrainer :initarg :constrainer :reader constrainer)
   (compiler :initarg :compiler :reader compiler)))

(defmethod print-object ((obj special-op) stream)
  (print-unreadable-object (obj stream)
    (format stream "OP ~A" (name obj))))

(defclass type-constructor ()
  ((func :initarg :func :reader func)))

(defclass macro ()
  ((impl :initarg :impl :reader impl)))

(defclass interface-func ()
  ((bl-type :initarg :bl-type :accessor bl-type)
   (interface :initarg :interface :reader interface)))
