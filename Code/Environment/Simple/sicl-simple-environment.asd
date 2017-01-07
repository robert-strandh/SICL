(cl:in-package #:asdf-user)

(defsystem :sicl-simple-environment
  :depends-on (:sicl-global-environment
	       :cleavir-compilation-policy)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "methods")))
