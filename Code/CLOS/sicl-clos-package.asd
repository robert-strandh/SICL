(cl:in-package #:asdf-user)

(defsystem :sicl-clos-package
  :depends-on (:sicl-additional-types
	       :sicl-additional-conditions
	       :cleavir-code-utilities)
  :serial t
  :components
  ((:file "packages")))
