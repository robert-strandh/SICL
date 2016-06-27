(cl:in-package #:asdf-user)

(defsystem :sicl-iteration-support
  :depends-on (:cleavir-code-utilities
               :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "utilities")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
