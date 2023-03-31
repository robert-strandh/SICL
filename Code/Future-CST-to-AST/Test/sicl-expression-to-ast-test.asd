(cl:in-package #:asdf-user)

(defsystem #:sicl-expression-to-ast-test
  :depends-on (#:sicl-expression-to-ast
               #:eclector-concrete-syntax-tree/test
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "environment")
   (:file "ast-from-file")))
