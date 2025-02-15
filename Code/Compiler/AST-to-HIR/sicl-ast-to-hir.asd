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
   (:file "let-temporary-ast")
   (:file "set-static-environment-ast")
   (:file "read-static-environment-ast")
   (:file "static-environment-ast")
   (:file "tagbody-with-variable-ast")
   (:file "go-with-variable-ast")
   (:file "block-with-variable-ast")
   (:file "return-from-with-variable-ast")
   (:file "special-variable-bind-ast")))
