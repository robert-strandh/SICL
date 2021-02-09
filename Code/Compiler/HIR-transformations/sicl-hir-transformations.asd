(cl:in-package #:asdf-user)

(defsystem sicl-hir-transformations
  :depends-on (#:cleavir-hir
               #:cleavir-ast-to-hir
               #:sicl-compiler-base
               #:sicl-run-time)
  :serial t
  :components
  ((:file "packages")
   (:file "preprocess-initialize-values")
   (:file "preprocess-multiple-value-call")
   (:file "eliminate-create-cell")
   (:file "eliminate-fetch")
   (:file "eliminate-cell-access")
   (:file "eliminate-fixed-to-multiple")
   (:file "eliminate-multiple-to-fixed")
   (:file "eliminate-append-values")
   (:file "eliminate-typeq")))
