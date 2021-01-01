(cl:in-package #:asdf-user)

(defsystem :cleavir-lir
  :depends-on (:cleavir-ir)
  :serial t
  :components
  ((:file "general")
   (:file "stack-instructions")))
