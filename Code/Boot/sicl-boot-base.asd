(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-base
  :depends-on (#:sicl-minimal-extrinsic-environment
               #:sicl-method-combination-support
               #:sicl-clos-macro-support
               #:sicl-package-support
               #:sicl-sequence-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")))
