(cl:in-package #:asdf-user)

(defsystem cleavir-type-inference
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "type-descriptor")
   (:file "filter")
   (:file "bag-equal")))
