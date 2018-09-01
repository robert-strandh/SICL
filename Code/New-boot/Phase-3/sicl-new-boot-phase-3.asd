(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-3
  :depends-on (#:sicl-new-boot-base
               #:sicl-minimal-extrinsic-environment
               #:sicl-clos-boot-support
               #:sicl-clos-macro-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "boot-phase-3")))
