(cl:in-package #:asdf-user)

(defsystem #:sicl-x86-64-registers
  :depends-on (#:cleavir-lir
               #:cleavir-mir
               #:cleavir-hir)
  :serial t
  :components
  ((:file "package")
   (:file "registers")
   (:file "register-maps")))
