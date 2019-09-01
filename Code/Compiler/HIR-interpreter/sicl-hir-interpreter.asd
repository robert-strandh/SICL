(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-interpreter
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "hir-interpreter")))
