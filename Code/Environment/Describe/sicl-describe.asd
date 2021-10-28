(cl:in-package #:asdf-user)

(defsystem #:sicl-describe
  :depends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "methods")
   (:file "describe-defun")))
