(cl:in-package #:asdf-user)

(defsystem #:sicl-reader
  :depends-on (#:eclector
               #:eclector-concrete-syntax-tree))

(defmethod asdf:operate :after
    ((operation asdf/lisp-action:load-op)
     (component (eql (asdf:find-system '#:sicl-reader)))
     &key)
  (delete-package '#:closer-common-lisp-user)
  (delete-package '#:closer-common-lisp)
  (delete-package '#:closer-mop))
