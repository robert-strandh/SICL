(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-target-sicl
  :serial t
  :components
  ((:file "packages")
   (:file "target")))
