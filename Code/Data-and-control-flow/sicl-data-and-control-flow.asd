(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow
  :depends-on (:sicl-global-environment
	       :cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")))
