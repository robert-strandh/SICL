(cl:in-package #:asdf-user)

(defsystem #:sicl-hir-visualizer
  :depends-on (#:sicl-hir
               #:mcclim
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "longest-path")
   (:file "utilities")
   (:file "variables")
   (:file "instruction-position")
   (:file "instruction-layout")
   (:file "datum-position")
   (:file "datum-layout")
   (:file "control-arc-layout")
   (:file "label")
   (:file "gui")
   (:file "commands")))
