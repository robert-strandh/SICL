(in-package #:asdf-user)

(defsystem :sicl-iteration
  :depends-on (:cleavir-code-utilities
	       :cleavir-internationalization
	       :sicl-iteration-support)
  :serial t
  :components
  ((:file "iteration")))
