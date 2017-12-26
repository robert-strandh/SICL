(cl:in-package #:asdf-user)

(defsystem :sicl-simple-readtable
  :depends-on (:sicl-readtable)
  :serial t
  :components
  ((:file "packages")
   (:file "readtable")
   (:file "methods")))

