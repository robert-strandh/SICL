(cl:in-package #:asdf-user)

(defsystem :cleavir-ast-transformations
  :depends-on (#:cleavir-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "clone")
   (:file "replace")
   (:file "parents")
   (:file "map-asts")
   (:file "remove-unused-block-asts")
   (:file "closure-conversion")))
