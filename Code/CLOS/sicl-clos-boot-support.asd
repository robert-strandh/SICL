(cl:in-package #:asdf-user)

;;;; The purpose of this system is to compile and load code that is
;;;; required for the SICL bootstrapping process.
;;;;
;;;; For one thing, we need for the SICL-specific metaobject accessor
;;;; generic functions to be defined so that they are automatically
;;;; imported into the global environments that are created during the
;;;; bootstrapping process.
;;;;
;;;; For a list of specified readers of method metaobjects, see
;;;; http://metamodular.com/CLOS-MOP/readers-for-method-metaobjects.html
;;;;
;;;; For a list of specified readers of slot-definition metaobjects, see
;;;; http://metamodular.com/CLOS-MOP/readers-for-slot-definition-metaobjects.html

(defsystem :sicl-clos-boot-support
  :depends-on (:sicl-clos-package)
  :serial t
  :components
  (;; This file contains the definition of the generic function
   ;; METHOD-FUNCTION.
   (:file "method-function-defgeneric")
   ;; This file contains the definition of the generic function
   ;; METHOD-GENERIC-FUNCTION.
   (:file "method-generic-function-defgeneric")
   ;; This file contains the definition of the generic function
   ;; METHOD-LAMBDA-LIST.
   (:file "method-lambda-list-defgeneric")
   ;; This file contains the definition of the generic function
   ;; METHOD-SPECIALIZERS.
   (:file "method-specializers-defgeneric")
   ;; This file contains the definition of the generic function
   ;; METHOD-QUALIFIERS.
   (:file "method-qualifiers-defgeneric")
   ;; This file contains the definition of the generic function
   ;; ACCESSOR-METHOD-SLOT-DEFINITION.
   (:file "accessor-method-slot-definition-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-ALLOCATION.
   (:file "slot-definition-allocation-defgeneric")))

;;  LocalWords:  metaobject accessor metaobjects accessors defgeneric
;;  LocalWords:  specializers
