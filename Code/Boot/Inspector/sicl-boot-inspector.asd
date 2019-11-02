(cl:in-package #:asdf-user)

(defsystem #:sicl-boot-inspector
  :depends-on (#:sicl-boot
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "inspect")
   (:file "object-state-class")))
