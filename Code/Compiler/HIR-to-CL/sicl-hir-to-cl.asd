(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-cl
  :depends-on (#:cleavir2-hir
               #:sicl-hir-transformations
               #:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "context")
   (:file "dynamic-environment")
   (:file "run-time")
   (:file "sort-functions")
   (:file "find-lexical-locations")
   (:file "translate")
   (:file "translate-cons-related-instructions")
   (:file "translate-multiple-value-related-instructions")
   (:file "translate-enter-instruction")
   (:file "hir-to-cl")))
