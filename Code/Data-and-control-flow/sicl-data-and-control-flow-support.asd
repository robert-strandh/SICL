(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-support
  :depends-on (:sicl-global-environment
	       :cleavir-code-utilities
	       :cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")))
