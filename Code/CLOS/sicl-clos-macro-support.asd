(cl:in-package #:asdf-user)

(defsystem :sicl-clos-macro-support
  :depends-on (:sicl-environment
               :sicl-clos-package)
  :serial t
  :components
  ((:file "with-slots-support")
   (:file "defclass-support")
   (:file "defgeneric-support")
   ;; The expansion of DEFMETHOD refers to MAKE-METHOD-LAMBDA, so we
   ;; include the definition of the generic function here.
   (:file "make-method-lambda-defgenerics")
   (:file "defmethod-support")))

