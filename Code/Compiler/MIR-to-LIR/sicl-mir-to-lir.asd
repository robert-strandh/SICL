(cl:in-package #:asdf-user)

(defsystem #:sicl-mir-to-lir
  :depends-on (#:cleavir2-lir
               #:cleavir2-mir
               #:cleavir2-hir
               #:sicl-ast-to-hir
               #:sicl-hir-to-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "registers")
   (:file "move-return-address")
   (:file "save-arguments")
   (:file "process-instructions")
   (:file "assignment")
   (:file "arguments")
   (:file "funcall")
   (:file "catch")
   (:file "bind")
   (:file "return")
   (:file "return-values")
   (:file "divide")
   (:file "memory")
   (:file "mir-to-lir")))

