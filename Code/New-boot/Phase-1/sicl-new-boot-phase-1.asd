(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-1
  :depends-on (#:sicl-minimal-extrinsic-environment
               #:sicl-clos-macro-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "load-accessor-defgenerics")
   (:file "define-mop-classes")
   (:file "boot-phase-1")))
