(cl:in-package #:asdf-user)

(defsystem :sicl-boot
  :depends-on (:cleavir-simple-value-numbering
	       :sicl-extrinsic-environment
	       :sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "message")
   (:file "boot")
   (:file "load")
   (:file "export-to-host")
   (:file "define-make-instance-defun")
   (:file "define-class-prototype-defun")
   (:file "environment-variables")
   (:file "phase1")
   (:file "define-find-class")
   (:file "create-bridge-classes")
   (:file "phase2")
   (:file "define-effective-slot-definition-class")
   (:file "phase3")
   (:file "fill")))
