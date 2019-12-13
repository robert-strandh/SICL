(cl:in-package #:asdf-user)

(defsystem #:sicl-compiler
  :depends-on (#:cleavir-code-utilities
               #:sicl-global-environment
               #:eclector
               #:cleavir-ast
               #:cleavir2-primop)
  :serial t
  :components
  ((:file "packages")
   (:file "compile-file")))
