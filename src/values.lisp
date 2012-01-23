(in-package #:bitlisp)

(defgeneric llvm (object))

(defclass value ()
  ((contents :initarg :contents :reader contents)
   (bl-type :initarg :bl-type :reader bl-type)
   (llvm :initarg :llvm :reader llvm)))

(defclass special-op ()
  ((resolver :initarg :resolver :reader resolver)
   (typer :initarg :typer :reader typer)
   (constrainer :initarg :constrainer :reader constrainer)
   (compiler :initarg :compiler :reader compiler)))

(defclass type-constructor ()
  ((func :initarg :func :reader func)))

(defclass macro ()
  ((impl :initarg :impl :reader impl)))

(defclass interface-func ()
  ((bl-type :initarg :bl-type :accessor bl-type)
   (interface :initarg :interface :reader interface)))
