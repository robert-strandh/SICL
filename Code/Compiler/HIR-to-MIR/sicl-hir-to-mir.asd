(cl:in-package #:common-lisp-user)

(asdf:defsystem #:sicl-hir-to-mir
  :depends-on (:cleavir-hir-to-mir)
  :serial t
  :components
  ((:file "packages")
   (:file "general")))
