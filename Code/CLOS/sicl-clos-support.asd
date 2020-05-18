(cl:in-package #:asdf-user)

(defsystem :sicl-clos-support
  :depends-on (:sicl-clos-package
               :sicl-global-environment)
  :serial t
  :components
  ((:file "make-method-lambda-support")
   (:file "make-method-lambda-defgenerics")
   (:file "make-method-lambda-defmethods")
   (:file "defmethod-support")
   (:file "make-method-for-generic-function")
   (:file "ensure-method")
   (:file "ensure-class-using-class-support")
   (:file "reader-writer-method-class-defgenerics")
   (:file "add-accessor-method")))
