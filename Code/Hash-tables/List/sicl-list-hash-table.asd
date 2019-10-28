(cl:in-package #:asdf-user)

(defsystem #:sicl-list-hash-table
  :depends-on (#:sicl-hash-table-base)
  :serial t
  :components
  ((:file "packages")
   (:file "list-hash-table-defclass")
   (:file "gethash-defmethod")
   (:file "setf-gethash-defmethod")
   (:file "hash-table-count-defmethod")
   (:file "remhash-defmethod")
   (:file "maphash-defmethod")
   (:file "clrhash-defmethod")))
