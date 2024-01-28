(cl:in-package #:asdf-user)

(defsystem "sicl-clos-ensure-metaobject-using-class"
  :serial t
  :components
  ((:file "ensure-generic-function-using-class")
   (:file "ensure-class-using-class")))
