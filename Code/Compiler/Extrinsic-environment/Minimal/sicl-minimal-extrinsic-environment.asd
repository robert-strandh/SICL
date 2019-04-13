(cl:in-package #:asdf-user)

(defsystem sicl-minimal-extrinsic-environment
  :depends-on (:concrete-syntax-tree
               :trivial-gray-streams
               :sicl-simple-environment
               :sicl-environment
               :sicl-loop-support
               :sicl-arithmetic
               :sicl-cons-support
               :sicl-clos-support
               :sicl-type-support
               :sicl-conditions
               :closer-mop
               :cleavir-generate-ast
               :cleavir-cst-to-ast
               :cleavir-ast-transformations
               :cleavir-ast-to-hir
               :cleavir-hir
               :cleavir-hir-transformations
               :cleavir-remove-useless-instructions
               :cleavir-basic-blocks
               :cleavir-meter
               :sicl-evaluation-and-compilation-support
               :sicl-data-and-control-flow-support
               :sicl-conditionals-support
               :sicl-iteration-support
               :sicl-source-tracking
               :eclector
               :eclector-concrete-syntax-tree
               :cleavir-equivalent-lexical-locations
               :cleavir-simple-value-numbering)
  :serial t
  :components
  ((:file "packages")
   (:file "host-load")
   (:file "environment")
   (:file "import-from-sicl-global-environment")
   (:file "import-from-host")
   (:file "runtime-environment")
   (:file "symbol-value")
   (:file "traced-funcall")
   (:file "parse-arguments")
   (:file "translate-hir")
   (:file "customization")
   (:file "eval")
   (:file "cst-eval")
   (:file "load")
   (:file "manual-definitions")
   (:file "fill")
   (:file "initialization")
   (:file "repl")))
