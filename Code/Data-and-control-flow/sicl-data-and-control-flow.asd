(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-data-and-control-flow
  :depends-on (:sicl-environment
	       :cleavir-internationalization)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "condition-reporters-english")))
