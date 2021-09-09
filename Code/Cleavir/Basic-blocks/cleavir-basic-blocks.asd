(cl:in-package #:asdf-user)

(defsystem #:cleavir-basic-blocks
  :depends-on (#:cleavir-utilities
               #:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "basic-blocks")))
