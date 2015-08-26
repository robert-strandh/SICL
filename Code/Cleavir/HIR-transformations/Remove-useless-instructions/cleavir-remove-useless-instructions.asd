(cl:in-package #:asdf-user)

(defsystem :cleavir-remove-useless-instructions
  :serial t
  :components
  ((:file "packages")
   (:file "remove-useless-instructions")))
