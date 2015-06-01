(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SICL-specific accessors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for specializer metaobjects other than classes.

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-DIRECT-SLOTS.  However, whereas
;;; for BUILT-IN-CLASS CLASS-DIRECT-SLOTS must return the empty list
;;; according to the MOP, this function returns the direct default
;;; slots of the built-in class.
(defgeneric direct-slots (class))

;;; This function is similar to CLASS-PRECEDENCE-LIST, except that
;;; CLASS-PRECEDENCE-LIST is specified to signal an error when the
;;; class is not finalized, which we accomplish by using a :BEFORE
;;; method.  However, during class finalization, we need to access the
;;; class precedence list in the two steps following its computation.
;;; Since at that point the class is not yet finalized, those two
;;; steps can not call CLASS-PRECEDENCE-LIST.  Our solution is to
;;; define an alternative reader for the same slot, named
;;; PRECEDENCE-LIST, and which does not signal an error if the class
;;; is not finalized.
(defgeneric precedence-list (class))

;;; This function is used by the class finalization protocol to set
;;; the precedence list of the class.
(defgeneric (setf precedence-list) (precedence-list class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-SLOTS.  However, whereas for
;;; BUILT-IN-CLASS CLASS-SLOTS must return the empty list according to
;;; the MOP, this function returns the effective default slots of the
;;; built-in class.
(defgeneric effective-slots (class))

;;; This function is used by the class finalization protocol to set
;;; the effective slots of the class. 
(defgeneric (setf c-slots) (effective-slots class))

;;; This function is used by ALLOCATE-INSTANCE and
;;; ALLOCATE-BUILT-IN-INSTANCE to determine the size of the instance
;;; to allocate.
(defgeneric instance-size (class))

;;; This function is used during class finalization, when the
;;; effective slots are known, and it is therefore also known what
;;; size the instances of this class should have.
(defgeneric (setf instance-size) (new-size class))

;;; This function returns the dependents of the class.
(defgeneric dependents (class))

;;; This function sets the dependents of the class.
(defgeneric (setf dependents) (dependents class))

;;; For a slot with :ALLOCATION :CLASS, this function returns a CONS
;;; cell where the CAR is used to store the value of the slot.
(defgeneric slot-definition-storage (direct-slot-definition))

(defgeneric (setf slot-definition) (slot-definition accessor-method))
