(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-1
  :depends-on (#:sicl-boot-base)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "import-from-host")
   (:file "pre-fill-environment")
   (:file "define-defmacro")
   (:file "define-backquote-macro")
   (:file "fill-environment")
   (:file "boot")))
