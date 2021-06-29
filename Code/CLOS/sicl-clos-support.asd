(cl:in-package #:asdf-user)

(defsystem :sicl-clos-support
  :depends-on (:sicl-clos-package
               :sicl-environment)
  :serial t
  :components
  ((:file "make-method-lambda-support")
   (:file "make-method-lambda-defgenerics")
   (:file "make-method-lambda-defmethods")
   (:file "defmethod-support")
   (:file "ensure-class-using-class-support")))
