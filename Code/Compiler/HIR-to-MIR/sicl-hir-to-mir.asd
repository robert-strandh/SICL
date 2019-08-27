(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-mir
  :depends-on (#:cleavir2-hir
               #:cleavir2-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "variables")
   (:file "enclose")
   (:file "cons")
   (:file "create-cell")
   (:file "utilities")
   (:file "general-instance")
   (:file "array")
   (:file "boxing")
   (:file "hir-to-mir")))
