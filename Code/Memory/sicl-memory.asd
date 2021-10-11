(cl:in-package #:asdf-user)

(defsystem #:sicl-memory
  :depends-on (#:cleavir-ast
               #:cleavir-cst-to-ast
               #:cleavir-ir
               #:cleavir-ast-to-hir
               #:sicl-hir-to-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "image")
   (:file "object-fixnum")
   (:file "pointer")))
