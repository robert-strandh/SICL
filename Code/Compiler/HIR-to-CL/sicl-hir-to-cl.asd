(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-cl
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "translate")
   (:file "translate-cons-related-instructions")
   (:file "translate-enter-instruction")
   (:file "hir-to-cl")))
