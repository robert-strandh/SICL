(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-extrinsic-environment
  :depends-on (:cleavir-environment)
  :serial t
  :components
  ((:file "packages")))
