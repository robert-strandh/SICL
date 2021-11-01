(cl:in-package #:asdf-user)

(defsystem #:sicl-form-to-ast
  :depends-on (#:sicl-cst-to-ast)
  :serial t
  :components
  ((:file "packages")
   (:file "form-to-ast")))
