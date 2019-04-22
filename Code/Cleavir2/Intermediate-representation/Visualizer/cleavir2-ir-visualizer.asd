(cl:in-package #:asdf-user)

(defsystem #:cleavir2-ir-visualizer
  :depends-on (#:cleavir2-ir
               #:cleavir2-hir
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
   (:file "gui")
   (:file "commands")))
