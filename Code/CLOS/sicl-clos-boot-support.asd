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
;;;;
;;;; For a list of specified readers of generic function metaobjects, see
;;;; http://metamodular.com/CLOS-MOP/readers-for-generic-function-metaobjects.html

(defsystem :sicl-clos-boot-support
  :depends-on (:sicl-clos-package)
  :serial t
  :components
  (;; This file contains support code for generic function
   ;; initialization.
   (:file "generic-function-initialization-support")
   ;; This file contains code for generating the discriminating
   ;; automaton of generic functions.
   (:file "discriminating-automaton")))

;;  LocalWords:  metaobject accessor metaobjects accessors defgeneric
;;  LocalWords:  specializers specializer superclasses subclasses
