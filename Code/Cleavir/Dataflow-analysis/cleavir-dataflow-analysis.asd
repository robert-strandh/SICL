(cl:in-package #:asdf-user)

(defsystem :cleavir-dataflow-analysis
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "dataflow-analysis")))
