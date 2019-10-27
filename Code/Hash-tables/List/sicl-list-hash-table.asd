(cl:in-package #:asdf-user)

(defsystem #:sicl-list-hash-table
  :depends-on (#:sicl-hash-table)
  :serial t
  :components
  ((:file "packages")
   (:file "list-hash-table-defclass")
   (:file "gethash-defmethod")
   (:file "setf-gethash-defmethod")
   (:file "hash-table-count-defmethod")))
