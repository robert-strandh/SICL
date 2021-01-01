(cl:in-package #:asdf-user)

(defsystem #:cleavir-value-hoisting-test
  :depends-on (#:cleavir-ast
               #:cleavir-cst-to-ast
               #:cleavir-ast-to-hir
               #:cleavir-value-hoisting)

  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "hir-from-form")
   (:file "make-load-form-using-client")
   (:file "test")))
