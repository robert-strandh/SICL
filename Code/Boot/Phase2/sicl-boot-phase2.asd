(cl:in-package #:asdf-user)

(defsystem :sicl-boot-phase2
  :depends-on (:sicl-boot-phase1)
  :serial t
  :components
  ((:file "packages")
   (:file "fill")
   (:file "environment")))
