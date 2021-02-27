(cl:in-package #:asdf-user)

(defsystem :cleavir-kildall
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "kildall")
   (:file "pool")
   (:file "dictionary")
   (:file "work-list")
   (:file "map-pool")
   (:file "iterate")
   (:file "initial-work")
   (:file "alist-pool")
   (:file "bitset")
   (:file "interfunction")))
