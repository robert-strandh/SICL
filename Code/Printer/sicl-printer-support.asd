(cl:in-package #:asdf-user)

(defsystem sicl-printer-support
  :serial t
  :components
  ((:file "packages")
   (:file "readtable-defclass")))
