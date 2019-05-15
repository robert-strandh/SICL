(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-to-cl
  :depends-on (#:cleavir2-hir
               #:sicl-hir-transformations
               #:sicl-alternative-extrinsic-environment
               #:closer-mop)
  :serial t
  :components
  ((:file "packages")
   (:file "basic-block")
   (:file "context")
   (:file "dynamic-environment")
   (:file "run-time")
   (:file "sort-functions")
   (:file "find-lexical-locations")
   (:file "translate")
   (:file "translate-cons-related-instructions")
   (:file "translate-environment-related-instructions")
   (:file "translate-multiple-value-related-instructions")
   (:file "translate-graph-instructions")
   (:file "translate-basic-block")
   (:file "translate-catch-instruction")
   (:file "translate-array-related-instructions")
   (:file "translate-boxing-related-instructions")
   (:file "translate-fixnum-related-instructions")
   (:file "hir-to-cl")))
