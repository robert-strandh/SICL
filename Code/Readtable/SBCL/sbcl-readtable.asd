(cl:in-package #:asdf-user)

(defsystem :sbcl-readtable
  :depends-on (:sicl-readtable)
  :serial t
  :components
  ((:file "packages")
   (:file "methods")))
