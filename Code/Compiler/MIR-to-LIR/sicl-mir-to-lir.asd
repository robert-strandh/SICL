(cl:in-package #:asdf-user)

(defsystem #:sicl-mir-to-lir
  :depends-on (#:cleavir2-lir)
  :serial t
  :components
  ((:file "packages")
   (:file "move-return-address")
   (:file "save-arguments")))
