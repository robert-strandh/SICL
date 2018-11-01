(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-base
  :depends-on (#:sicl-minimal-extrinsic-environment
               #:sicl-method-combination-support
               #:sicl-clos-macro-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")))
