(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-cl
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "translate")
   (:file "hir-to-cl")))
