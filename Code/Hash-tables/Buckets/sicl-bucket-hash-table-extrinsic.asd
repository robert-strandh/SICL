(cl:in-package #:asdf-user)

(defsystem #:sicl-bucket-hash-table-extrinsic
  :depends-on (#:sicl-hash-table-base-extrinsic)
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "bucket-hash-table-defclass")
   (:file "bucket-hash-table")))
