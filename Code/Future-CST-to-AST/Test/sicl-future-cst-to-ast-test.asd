(cl:in-package #:asdf-user)

(defsystem #:sicl-future-cst-to-ast-test
  :depends-on (#:sicl-future-cst-to-ast
               #:eclector-concrete-syntax-tree/test
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "ast-from-file")))
