(cl:in-package #:asdf-user)

(defsystem :sicl-clos-test
  :depends-on (#:closer-mop)
  :serial t
  :components
  ((:file "package")
   (:file "import-from-host")
   (:file "../generic-function-defclass")
   (:file "../standard-generic-function-defclass")))

