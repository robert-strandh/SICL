(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-3"
  :depends-on ("sicl-new-boot-phase-2")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "configuration")
   (:file "macro-programming")
   (:file "import-from-host")
   (:file "define-class-of-and-stamp")
   (:file "load-predicament")
   (:file "boot")))
