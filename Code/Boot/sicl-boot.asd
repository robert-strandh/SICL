(cl:in-package #:asdf-user)

(defsystem :sicl-boot
  :depends-on (:sicl-extrinsic-environment
	       :sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "message")
   (:file "boot")
   (:file "load")
   (:file "export-to-host")
   (:file "customize-environments")
   (:file "define-make-instance-defun")
   (:file "define-class-prototype-defun")
   (:file "phase1")
   (:file "define-direct-slot-definition-class")
   (:file "define-find-class")
   (:file "create-bridge-class-accessors")
   (:file "create-bridge-classes")
   (:file "phase2")
   (:file "define-effective-slot-definition-class")
   (:file "phase3")
   (:file "fill")))
