(cl:in-package #:asdf-user)

(defsystem #:sicl-cst-to-ast-s-expression-syntax
  :depends-on (#:s-expression-syntax)
  :serial t
  :components
  ((:file "packages")))
