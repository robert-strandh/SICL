(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-interpreter
  :depends-on (#:cleavir2-hir
               #:sicl-global-environment)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "dynamic-environment")
   (:file "run-time")
   (:file "argument-processing-instructions")
   (:file "array-related-instructions")
   (:file "boxing-related-instructions")
   (:file "cons-related-instructions")
   (:file "fixnum-related-instructions")
   (:file "multiple-value-related-instructions")
   (:file "hir-interpreter")))
