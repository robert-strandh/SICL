(cl:in-package #:asdf-user)

(defsystem "sicl-clos-ensure-metaobject-using"
  :serial t
  :components
  ((:file "ensure-generic-function-using-class")
   (:file "ensure-class-using-class")
   (:file "ensure-method-using-generic-function")
   (:file "ensure-method-combination-template-using-class")))
