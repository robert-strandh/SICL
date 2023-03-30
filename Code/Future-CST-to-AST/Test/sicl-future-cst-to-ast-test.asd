(cl:in-package #:asdf-user)

(defsystem #:sicl-future-cst-to-ast-test
  :depends-on (#:sicl-future-cst-to-ast
               #:eclector-concrete-syntax-tree/test
               #:clearcut-implementation-s-expression
               #:clearcut-implementation-concrete-syntax-tree
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "environment")
   (:file "ast-from-file")))
