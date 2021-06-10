(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-phase-6
  :depends-on (#:sicl-boot-base
               #:sicl-clos-boot-support)
  :serial t
  :components
  ((:file "packages")
   (:file "enable-deftype")
   (:file "enable-printing")
   (:file "enable-reading")
   (:file "enable-conditions")
   (:file "load-closer-mop")
   (:file "load-eclector")
   (:file "load-ctype")
   (:file "boot")))
