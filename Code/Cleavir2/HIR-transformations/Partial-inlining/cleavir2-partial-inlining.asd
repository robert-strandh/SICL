(cl:in-package #:asdf-user)

(defsystem :cleavir2-partial-inlining
  :depends-on (:cleavir-hir
               :cleavir2-hir-transformations
               :cleavir2-remove-useless-instructions)
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
