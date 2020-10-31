(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-0
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "import-from-host")
   (:file "define-defmacro")
   (:file "define-backquote-macro")
   (:file "fill-environment")
   (:file "environment")
   (:file "boot")))
