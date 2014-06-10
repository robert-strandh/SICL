(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-arithmetic
  :serial t
  :components
  ((:file "packages")
   (:file "support")))

