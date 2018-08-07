(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-support
  :depends-on (:sicl-global-environment
	       :cleavir-code-utilities
	       :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")
   (:file "shiftf-support")
   (:file "psetf-support")
   (:file "rotatef-support")))
