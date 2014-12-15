(cl:in-package #:common-lisp-user)

(asdf:defsystem :cleavir-hir-transformations
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "compute-nesting-depth")
   (:file "inline-calls")
   (:file "static-few-assignments")
   (:file "type-inference")
   (:file "eliminate-typeq")
   (:file "simplify-box-unbox")
   (:file "segregate-lexicals")
   (:file "hir-transformations")))
