(cl:in-package #:asdf-user)

(defsystem #:sicl-new-boot-base
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")))
