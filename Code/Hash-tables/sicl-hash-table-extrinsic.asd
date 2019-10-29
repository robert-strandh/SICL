(cl:in-package #:asdf-user)

(defsystem :sicl-hash-table-extrinsic
  :depends-on (#:sicl-hash-table-base-extrinsic
               #:sicl-list-hash-table-extrinsic)
  :serial t
  :components
  ((:file "with-hash-table-iterator-defmacro")))
