(cl:in-package #:asdf-user)

(defsystem :cleavir-partial-inlining
  :depends-on (:cleavir-hir
               :cleavir-hir-transformations
               :cleavir-remove-useless-instructions)
  :serial t
  :components
  ((:file "packages")
   (:file "variables")
   (:file "generic-functions")
   (:file "mapping")
   (:file "worklist-item")
   (:file "copy-function")
   (:file "inline-one-instruction")
   (:file "inline-function")
   (:file "full-inlining-pass")))
