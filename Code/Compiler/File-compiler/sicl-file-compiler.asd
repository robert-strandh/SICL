(cl:in-package #:asdf-user)

(defsystem #:sicl-file-compiler
  :depends-on (#:sicl-compiler-base
               #:sicl-cst-to-ast
               #:sicl-environment
               #:eclector
               #:sicl-source-tracking)
  :serial t
  :components
  ((:file "ast-from-file-or-stream")))
