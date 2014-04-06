(cl:in-package #:common-lisp-user)

(asdf:defsystem :clvm-io
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
