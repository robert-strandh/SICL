(cl:in-package #:asdf-user)

(defsystem :cleavir-remove-useless-instructions
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "remove-useless-instructions")))
