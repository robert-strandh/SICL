(cl:in-package #:asdf-user)

(defsystem :sicl-clos-support
  :depends-on (:sicl-clos-package
	       :sicl-global-environment)
  :serial t
  :components
  ((:file "ensure-generic-function-using-class-support")
   (:file "ensure-method")))
