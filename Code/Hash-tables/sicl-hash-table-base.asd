(cl:in-package #:asdf-user)

(defsystem :sicl-hash-table-base
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "hash-table-defclass")
   (:file "sxhash")
   (:file "sxhash-intrinsic")
   (:file "hashing-hash-table")
   (:file "make-hash-table")))
