(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-simple-environment
  :depends-on (:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "environment")
   (:file "methods")))
