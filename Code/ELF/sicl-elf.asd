(cl:in-package #:asdf-user)

(defsystem #:sicl-elf
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")))
