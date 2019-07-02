(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-inspector
  :depends-on (#:sicl-boot
               #:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "print-length")
   (:file "short-description")
   (:file "display-object")
   (:file "gui")))
