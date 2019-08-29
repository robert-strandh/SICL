(cl:in-package #:asdf-user)

(defsystem #:sicl-mir-to-lir
  :depends-on (#:cleavir2-lir
               #:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "registers")
   (:file "move-return-address")
   (:file "save-arguments")))
