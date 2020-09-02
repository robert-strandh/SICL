(cl:in-package #:asdf-user)

(defsystem :sicl-conditions-support
  :serial t
  :components
  ((:file "packages")
   (:file "define-condition-support")
   (:file "handlers-utilities")))
