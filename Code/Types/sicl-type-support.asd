(cl:in-package #:asdf-user)

(defsystem :sicl-type-support
  :depends-on (:closer-mop
	       :sicl-global-environment)
  :components
  ((:file "packages")
   (:file "float-types")
   (:file "expand")))
