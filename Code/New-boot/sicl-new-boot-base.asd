(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-base
  :depends-on (#:sicl-minimal-extrinsic-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")))
