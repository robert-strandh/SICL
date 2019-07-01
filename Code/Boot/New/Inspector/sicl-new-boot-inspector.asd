(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-inspector
  :depends-on (#:sicl-new-boot
               #:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "print-length")
   (:file "short-description")
   (:file "display-object")
   (:file "gui")))
