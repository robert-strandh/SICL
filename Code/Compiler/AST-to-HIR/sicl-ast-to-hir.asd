(cl:in-package #:asdf-user)

(defsystem #:sicl-ast-to-hir
  :depends-on (#:cleavir2-ast-to-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "ast-to-hir")))
