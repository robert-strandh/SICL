(cl:in-package #:asdf-user)

(defsystem #:cleavir2-ast-visualizer
  :depends-on (#:cleavir2-ast
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
