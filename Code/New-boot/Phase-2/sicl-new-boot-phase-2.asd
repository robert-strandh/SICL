(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-2
  :depends-on (#:sicl-minimal-extrinsic-environment
               #:sicl-clos-macro-support)
  :serial t
  :components
  ((:file "packages")))
