(cl:in-package #:asdf-user)

(defsystem sicl-hir-transformations
  :depends-on (#:cleavir-hir
               #:cleavir-ast-to-hir
               #:sicl-compiler-base
               #:sicl-run-time)
  :serial t
  :components
  ((:file "packages")
   (:file "preprocess-unwind")
   (:file "preprocess-catch")
   (:file "preprocess-bind")
   (:file "preprocess-initialize-values")
   (:file "preprocess-multiple-value-call")
   (:file "hoist-fdefinitions")
   (:file "find-named-call-instructions")
   (:file "eliminate-create-cell")
   (:file "eliminate-fetch")
   (:file "eliminate-cell-access")
   (:file "process-constant-inputs")
   (:file "eliminate-fixed-to-multiple")
   (:file "eliminate-multiple-to-fixed")
   (:file "eliminate-append-values")
   (:file "eliminate-typeq")))
