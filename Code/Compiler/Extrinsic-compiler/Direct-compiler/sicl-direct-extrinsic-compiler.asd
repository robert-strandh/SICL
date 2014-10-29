(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-direct-extrinsic-compiler
  :depends-on (:sicl-extrinsic-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "compile")))
