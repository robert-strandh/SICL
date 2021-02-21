(cl:in-package #:asdf-user)

(defsystem :sicl-hash-table-base-extrinsic
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "hash-table-defclass-extrinsic")
   (:file "generic-functions")
   (:file "sxhash")
   (:file "hashing-hash-table")
   (:file "make-hash-table")))
