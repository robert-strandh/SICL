(cl:in-package #:asdf-user)

;;;; The purpose of this system is to compile and load code that is
;;;; required for the SICL bootstrapping process.
;;;;
;;;; For one thing, we need for the SICL-specific metaobject accessor
;;;; generic functions to be defined so that they are automatically
;;;; imported into the global environments that are created during the
;;;; bootstrapping process.

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
   ;; This file contains definitions of generic functions that are
   ;; slot accessors for method metaobjects.
   (:file "method-readers-defgenerics")))

;;  LocalWords:  metaobject accessor metaobjects accessors
