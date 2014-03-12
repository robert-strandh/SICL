(cl:in-package #:sicl-boot-phase1)

(define-symbol-macro *t*
    (find-bridge-class t))

(define-symbol-macro *standard-object*
    (find-bridge-class 'cl:standard-object))

(defvar *funcallable-standard-object*)

(defvar *standard-class*
  (find-class 'standard-class))

(defvar *funcallable-standard-class*
  (find-class 'funcallable-standard-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables *STANDARD-READER-METHOD* and *STANDARD-WRITER-METHOD* 
;;;
;;; These variables are referred to by the default methods on the
;;; generic functions READER-METHOD-CLASS and WRITER-METHOD-CLASS that
;;; are defined later in phase 1.  These functions are called when the
;;; MOP hierarchy is loaded in phase 2 in order to create bridge
;;; classes.  Creating a bridge class (which is a host instance)
;;; implicitly creates bridge generic functions (which are also host
;;; instances) for the accessors, and bridge methods on those bridge
;;; generic functions.  Since bridge methods are also host instances,
;;; the values of the variables must be host classes. 

(defvar *standard-reader-method*
  (find-class 'standard-reader-method))

(defvar *standard-writer-method*
  (find-class 'standard-writer-method))

(defvar *standard-direct-slot-definition*
  (find-class 'standard-direct-slot-definition))

(defvar *standard-effective-slot-definition*
  (find-class 'standard-effective-slot-definition))

(defvar *standard-reader-method*
  (find-class 'standard-reader-method*))

(defvar *standard-writer-method*
  (find-class 'standard-writer-method*))
