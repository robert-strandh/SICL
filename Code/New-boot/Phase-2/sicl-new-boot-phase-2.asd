(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-2"
  :depends-on ("sicl-new-boot-phase-1")
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")
   (:file "boot")))
