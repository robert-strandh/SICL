(cl:in-package #:asdf-user)

(defsystem #:sicl-filename
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "pathname-defclass")))
