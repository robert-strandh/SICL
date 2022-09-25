(cl:in-package #:asdf-user)

(defsystem #:sicl-cst-to-ast-s-expression-syntax
  :depends-on (#:s-expression-syntax.concrete-syntax-tree
               #:concrete-syntax-tree-destructuring
               #:cleavir-ast
               #:cleavir-ast-transformations
               #:cleavir-primop
               #:cleavir-literals
               #:trucler-reference
               #:stealth-mixin)
  :serial t
  :components
  ((:file "packages")))
