(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-base
  :depends-on (#:sicl-alternative-extrinsic-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "utilities")))
