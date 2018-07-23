(cl:in-package #:asdf-user)

(defsystem #:cleavir-ir-visualizer
  :depends-on (#:cleavir-ir
               #:cleavir-hir
               #:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "gui")))
