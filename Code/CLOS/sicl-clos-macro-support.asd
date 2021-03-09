(cl:in-package #:asdf-user)

(defsystem :sicl-clos-macro-support
  :depends-on (:sicl-environment
               :sicl-clos-package)
  :serial t
  :components
  ((:file "with-slots-support")
   (:file "defclass-support")
   ;; The expansion of DEFGENERIC refers to the accessor
   ;; INITIAL-METHODS used to handle the :METHODS option of DEFCLASS,
   ;; so we include the definition of both the slot reader and the
   ;; slot writer here.
   (:file "initial-methods-defgeneric")
   (:file "setf-initial-methods-defgeneric")
   (:file "defgeneric-support")
   ;; The expansion of DEFMETHOD refers to CLASS-PROTOTYPE to compute
   ;; the argument to MAKE-METHOD-LAMBDA, so we include the definition
   ;; of the generic function so as to avoid warnings.
   (:file "class-prototype-defgeneric")
   ;; The expansion of DEFMETHOD refers to MAKE-METHOD-LAMBDA, so we
   ;; include the definition of the generic function here.
   (:file "make-method-lambda-defgenerics")
   (:file "defmethod-support")))

