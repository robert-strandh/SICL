(cl:in-package #:asdf-user)

(defsystem :sicl-data-and-control-flow-support-clostrum
  :depends-on (:sicl-global-environment
	       :cleavir-code-utilities
	       :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "conditions")
   (:file "defun-support")
   (:file "shiftf-support")
   (:file "psetf-support")
   (:file "rotatef-support")
   (:file "destructuring-bind-support")))
