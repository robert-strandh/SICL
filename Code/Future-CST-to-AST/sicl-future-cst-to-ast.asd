(cl:in-package #:asdf-user)

(defsystem #:sicl-future-cst-to-ast
  :depends-on (#:concrete-syntax-tree
               #:concrete-syntax-tree-destructuring
               #:iconoclast
               #:iconoclast-builder
               #:trucler-reference
               #:clearcut-implementation-s-expression
               #:clearcut-implementation-concrete-syntax-tree)
  :serial t
  :components
  ((:file "packages")
   (:file "environment-augmentation")
   (:file "environment-query")
   (:file "generic-functions")
   (:file "eval")
   (:file "builder")
   (:file "let")
   (:file "progn")
   (:file "variables")
   (:file "convert-constant")
   (:file "convert-variable")
   (:file "convert")
   (:file "cst-to-ast")))
