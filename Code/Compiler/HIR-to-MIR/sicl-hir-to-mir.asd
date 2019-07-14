(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-mir
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "generic-functions")
   (:file "fetch")
   (:file "cons")
   (:file "cell")
   (:file "utlities")
   (:file "general-instance")
   (:file "array")
   (:file "hir-to-mir")))
