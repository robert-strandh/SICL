(cl:in-package #:asdf-user)

(defsystem :sicl-hash-table-base
  :serial t
  :components
  ((:file "packages")
   (:file "hash-table-defclass")
   (:file "generic-functions")
   (:file "sxhash")
   (:file "sxhash-intrinsic")
   (:file "hashing-hash-table")
   (:file "make-hash-table")))
