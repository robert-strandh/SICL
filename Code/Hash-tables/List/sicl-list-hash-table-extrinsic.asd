(cl:in-package #:asdf-user)

(defsystem #:sicl-list-hash-table-extrinsic
  :depends-on (#:sicl-hash-table-base-extrinsic)
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "list-hash-table-defclass-extrinsic")
   (:file "gethash-defmethod")
   (:file "setf-gethash-defmethod")
   (:file "hash-table-count-defmethod")
   (:file "remhash-defmethod")
   (:file "maphash-defmethod")
   (:file "clrhash-defmethod")))
