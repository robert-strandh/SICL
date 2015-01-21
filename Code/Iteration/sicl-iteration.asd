(in-package #:asdf-user)

(defsystem :sicl-iteration
  :depends-on (:cleavir-code-utilities
	       :cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "utilities")
   (:file "iteration")
   (:file "condition-reporters-en")
   (:file "docstrings-en")))
