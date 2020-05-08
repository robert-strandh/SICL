(cl:in-package #:asdf-user)

(defsystem sicl-boot-trace
  :depends-on (:sicl-boot)
  :serial t
  :components
  ((:file "packages")
   (:file "trace")))
