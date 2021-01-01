(cl:in-package #:asdf-user)

(defsystem #:cleavir-ir-visualizer
  :depends-on (#:cleavir-ir
               #:cleavir-hir
               #:cleavir-mir
               #:cleavir-lir
               #:mcclim
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "longest-path")
   (:file "variables")
   (:file "instruction-position")
   (:file "instruction-layout")
   (:file "datum-position")
   (:file "datum-layout")
   (:file "control-arc-layout")
   (:file "label")
   (:file "gui")
   (:file "commands")))
