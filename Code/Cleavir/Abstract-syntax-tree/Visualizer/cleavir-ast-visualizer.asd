(cl:in-package #:asdf-user)

(defsystem #:cleavir-ast-visualizer
  :depends-on (#:cleavir-ast
               #:mcclim
               #:clouseau)
  :serial t
  :components
  ((:file "packages")
   (:file "profiles")
   (:file "layout")
   (:file "color")
   (:file "labels")
   (:file "gui")))
