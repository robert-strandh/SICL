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
;;;;
;;;; For a list of specified readers of class metaobjects, see
;;;; http://metamodular.com/CLOS-MOP/readers-for-class-metaobjects.html

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
   (:file "slot-definition-allocation-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-INITARGS.
   (:file "slot-definition-initargs-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-INITFORM.
   (:file "slot-definition-initform-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-INITFUNCTION.
   (:file "slot-definition-initfunction-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-NAME.
   (:file "slot-definition-name-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-TYPE.
   (:file "slot-definition-type-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-READERS.
   (:file "slot-definition-readers-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-WRITERS.
   (:file "slot-definition-writers-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SLOT-DEFINITION-LOCATION.
   (:file "slot-definition-location-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SPECIALIZER-DIRECT-GENERIC-FUNCTIONS.
   (:file "specializer-direct-generic-functions-defgeneric")
   ;; This file contains the definition of the generic function
   ;; SPECIALIZER-DIRECT-METHODS.
   (:file "specializer-direct-methods-defgeneric")
   ;; This file contains the definition of the generic function
   ;; EQL-SPECIALIZER-OBJECT.
   (:file "eql-specializer-object-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-DEFAULT-INITARGS.
   (:file "class-default-initargs-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-DIRECT-DEFAULT-INITARGS.
   (:file "class-direct-default-initargs-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-NAME.
   (:file "class-name-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-DIRECT-SUPERCLASSES.
   (:file "class-direct-superclasses-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-DIRECT-SLOTS.
   (:file "class-direct-slots-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-DIRECT-SUBCLASSES.
   (:file "class-direct-subclasses-defgeneric")
   ;; This file contains the definition of the generic function
   ;; CLASS-SLOTS.
   (:file "class-slots-defgeneric")))

;;  LocalWords:  metaobject accessor metaobjects accessors defgeneric
;;  LocalWords:  specializers specializer superclasses subclasses
