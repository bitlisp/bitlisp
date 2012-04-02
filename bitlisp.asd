(asdf:defsystem bitlisp
  :serial t
  :depends-on (#:llvm #:alexandria #:cffi #:yacc #:parse-number)
  :pathname "src/"
  :components
  ((:file "package")
   (:file "utils")
   (:file "symbols")
   (:file "reader")
   (:file "environments")
   (:file "types")
   (:file "values")
   (:file "unification")
   (:file "typechecking")
   (:file "modules")
   (:file "codegen")
   (:file "specials")
   (:file "primitives")))
