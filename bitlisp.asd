(asdf:defsystem bitlisp
  :serial t
  :depends-on (#:llvm #:alexandria #:cffi #:yacc #:parse-number)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "symbols")
   (:file "reader")
   (:file "environments")
   (:file "types")
   (:file "values")
   (:file "unification")
   (:file "typechecking")
   (:file "primitives")))
