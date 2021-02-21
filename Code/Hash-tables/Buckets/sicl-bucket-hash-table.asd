(cl:in-package #:asdf-user)

(defsystem #:sicl-bucket-hash-table
  :depends-on (#:sicl-hash-table-base)
  :serial t
  :components
  ((:file "packages")
   (:file "bucket-hash-table-defclass")
   (:file "bucket-hash-table")))
