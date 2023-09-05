(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot"
  :depends-on ("common-boot")
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")))
