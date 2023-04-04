(cl:in-package #:asdf-user)

(defsystem #:sicl-expression-to-ast
  :depends-on (#:concrete-syntax-tree
               #:concrete-syntax-tree-destructuring
               #:iconoclast
               #:iconoclast-builder
               #:trucler-reference
               #:sicl-extended-clearcut)
  :serial t
  :components
  ((:file "packages")
   (:file "environment-augmentation")
   (:file "environment-query")
   (:file "generic-functions")
   (:file "utilities")
   (:file "eval")
   (:file "builder")
   (:file "declarations")
   (:file "let")
   (:file "progn")
   (:file "block")
   (:file "return-from")
   (:file "catch")
   (:file "throw")
   (:file "eval-when")
   (:file "tagbody")
   (:file "go")
   (:file "variables")
   (:file "convert-constant")
   (:file "convert-variable")
   (:file "convert-with-description")
   (:file "convert")
   (:file "convert-ast")
   (:file "expression-to-ast")))
