(cl:in-package #:asdf-user)

(defsystem :sicl-extrinsic-hir-interpreter
  :depends-on (:sicl-global-environment
	       :sicl-simple-environment
	       :cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "interpreter")
   (:file "instructions")))
