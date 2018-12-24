(cl:in-package #:asdf-user)

(defsystem #:cleavir-ast-visualizer
  :depends-on (#:cleavir-ast
               #:mcclim)
  :serial t
  :components
  ((:file "packages")
   (:file "gui")))
