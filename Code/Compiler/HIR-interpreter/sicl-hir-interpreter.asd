(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-interpreter
  :depends-on (#:cleavir2-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "utilities")
   (:file "argument-processing-instructions")
   (:file "array-related-instructions")
   (:file "hir-interpreter")))
