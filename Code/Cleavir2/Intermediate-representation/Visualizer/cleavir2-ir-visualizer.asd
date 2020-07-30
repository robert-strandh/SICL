(cl:in-package #:asdf-user)

(defsystem #:cleavir2-ir-visualizer
  :depends-on (#:cleavir2-ir
               #:cleavir2-hir
               #:cleavir2-mir
               #:cleavir2-lir
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
