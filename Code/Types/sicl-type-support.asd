(cl:in-package #:asdf-user)

(defsystem :sicl-type-support
  :depends-on (:sicl-environment
	       :closer-mop
	       :sicl-global-environment)
  :components
  ((:file "packages")
   (:file "float-types")
   (:file "expand")
   (:file "typep-compound")
   (:file "typep-atomic")))
