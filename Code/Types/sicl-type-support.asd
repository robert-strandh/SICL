(cl:in-package #:asdf-user)

(defsystem :sicl-type-support
  :depends-on (:sicl-environment
	       :closer-mop
	       :sicl-global-environment)
  :components
  ((:file "packages")
   (:file "typep-compound")
   (:file "typep-atomic")))
