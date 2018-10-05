(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-phase-0
  :depends-on (#:sicl-new-boot-base
               #:sicl-clos-macro-support)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "boot-phase-0")))
