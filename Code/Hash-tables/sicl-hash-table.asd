(cl:in-package #:asdf-user)

(defsystem :sicl-hash-table
  :serial t
  :components
  ((:file "packages")
   (:file "hash-table-defclass")))
