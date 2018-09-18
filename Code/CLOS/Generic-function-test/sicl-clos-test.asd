(cl:in-package #:asdf-user)

(defsystem :sicl-clos-test
  :depends-on (#:closer-mop)
  :serial t
  :components
  ((:file "package")
   (:file "funcallable-standard-class-defclass")
   (:file "../standard-generic-function-defclass")))

