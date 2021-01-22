(cl:in-package #:asdf-user)

(defsystem #:sicl-linear-probing-hash-table-extrinsic
  :depends-on (#:sicl-hash-table-base-extrinsic)
  :serial t
  :components ((:file "package-extrinsic")
               (:file "linear-hash-table-defclass")
               (:file "metadata-table")
               (:file "linear-probing-hash-table")))
