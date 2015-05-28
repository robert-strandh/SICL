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
   (:file "fill1")
   (:file "fill")))
