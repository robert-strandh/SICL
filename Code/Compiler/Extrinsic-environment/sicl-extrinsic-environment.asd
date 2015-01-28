(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-environment
  :depends-on (:sicl-simple-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "environment-defclass")
   (:file "fill")
   (:file "initialization")))
