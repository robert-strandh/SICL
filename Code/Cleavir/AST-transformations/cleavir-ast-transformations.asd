(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-transformations
  :depends-on (:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "clone")
   (:file "replace")
   (:file "hoist-load-time-value")
   (:file "closure-conversion")))
