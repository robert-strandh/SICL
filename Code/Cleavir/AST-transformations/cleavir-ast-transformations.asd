(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-ast-transformations
  :depends-on (:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "clone")
   (:file "hoist-load-time-value")))
