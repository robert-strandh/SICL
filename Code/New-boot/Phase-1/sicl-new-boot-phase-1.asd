(cl:in-package #:asdf-user)

(defsystem "sicl-new-boot-phase-1"
  :depends-on ("sicl-new-boot-shared")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "configuration")
   (:file "macro-programming")
   (:file "import-from-host")
   (:file "load-predicament")
   (:file "define-my-make-instance")
   (:file "load-ctype")
   (:file "boot")))
