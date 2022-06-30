(cl:in-package #:asdf-user)

(defsystem #:sicl-arithmetic-compile-time
  :depends-on (#:sicl-arithmetic-base)
  :serial t
  :components
  ((:file "incf-decf-defmacro")))
