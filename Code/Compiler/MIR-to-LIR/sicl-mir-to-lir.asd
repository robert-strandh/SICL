(cl:in-package #:asdf-user)

(defsystem #:sicl-mir-to-lir
  :depends-on (#:cleavir-lir
               #:cleavir-mir
               #:cleavir-hir
               #:sicl-ast-to-hir
               #:sicl-hir-to-mir
               #:sicl-register-allocation)
  :serial t
  :components
  ((:file "packages")
   (:file "finish-lir")
   (:file "call-instruction")
   (:file "mir-to-lir")))
