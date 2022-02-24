(cl:in-package #:asdf-user)

(defsystem #:sicl-compiler-base
  :depends-on (#:sicl-primop
               #:eclector-concrete-syntax-tree
               #:sicl-source-tracking
               #:sicl-ast
               #:sicl-ir)
  :components
  ((:file "packages")
   (:file "cst-from-file-or-stream")
   (:file "ensure-literal")))
