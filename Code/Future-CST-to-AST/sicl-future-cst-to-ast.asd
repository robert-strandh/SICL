(cl:in-package #:asdf-user)

(defsystem #:sicl-future-cst-to-ast
  :depends-on (#:concrete-syntax-tree
               #:concrete-syntax-tree-destructuring
               #:iconoclast
               #:iconoclast-builder
               #:trucler-reference)
  :serial t
  :components
  ((:file "packages")
   (:file "environment-augmentation")
   (:file "environment-query")
   (:file "generic-functions")
   (:file "builder")
   (:file "progn")
   (:file "variables")
   (:file "cst-to-ast")))
