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
   (:file "customize-c1")
   (:file "define-make-instance-defun")
   (:file "define-ensure-generic-function-defun")
   (:file "define-class-prototype-defun")
   (:file "define-generic-function-method-class-defun")
   (:file "fill1")
   (:file "customize-r2")
   (:file "fill")))
