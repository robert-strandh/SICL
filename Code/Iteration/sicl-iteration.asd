(in-package #:asdf-user)

(defsystem :sicl-iteration
  :depends-on (:cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "iteration")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
