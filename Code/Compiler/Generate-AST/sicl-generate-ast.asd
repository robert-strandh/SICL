(cl:in-package #:common-lisp-user)

(asdf:defsystem :sicl-generate-ast
  :serial t
  :components
  ((:file "packages")
   (:file "generate-ast")))
