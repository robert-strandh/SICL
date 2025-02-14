(cl:in-package #:asdf-user)

(defsystem "sicl-ast-to-hir"
  :depends-on ("iconoclast"
               "sicl-hir")
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "registers")
   (:file "translate")
   (:file "variable-reference-ast")
   (:file "progn-ast")
   (:file "let-temporary-ast")))
