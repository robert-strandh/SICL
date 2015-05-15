(cl:in-package #:asdf-user)

(defsystem :sicl-boot-phase1
  :depends-on (:sicl-extrinsic-environment
	       :sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "fill")
   (:file "environment")))
