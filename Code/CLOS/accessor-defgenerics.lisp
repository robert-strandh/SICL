(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SICL-specific accessors.

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric gf-documentation (generic-function))

;;; This function sets the methods of the generic function.
(defgeneric (setf methods) (new-methods generic-function))

;;; This function returns the specializer profile of the generic function.
;;; FIXME: say more.
(defgeneric specializer-profile (generic-function))

;;; This function sets the specializer profile of the generic function.
;;; FIXME: say more.
(defgeneric (setf specializer-profile) (new-specializer-profile generic-function))

;;; This function is called by ADD-METHOD and REMOVE-METHOD to assign
;;; the generic function to which the method is associated.
(defgeneric (setf method-generic-function) (generic-function method))

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric method-documentation (generic-function))

;;; This function will be removed later.  For now it plays the role of
;;; the function (SETF CL:DOCUMENTATION). 
(defgeneric (setf method-documentation) (documentation generic-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for specializer metaobjects other than classes.

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of generic functions
;;; having SPECIALIZER as a specializer.
(defgeneric (setf direct-generic-functions) (new-generic-functions specializer))

;;; This function returns the unique number of the class, assigned
;;; when the class is initialized or reinitialized.
(defgeneric unique-number (class))

;;; This function sets the direct methods of the class.
(defgeneric (setf c-direct-methods) (direct-methods class))

(defgeneric default-initargs (class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-DIRECT-DEFAULT-INITARGS.  However,
;;; whereas for BUILT-IN-CLASS CLASS-DIRECT-DEFAULT-INITARGS must
;;; return the empty list according to the MOP, this function returns
;;; the direct default initargs of the built-in class.
(defgeneric direct-default-initargs (class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-DIRECT-SLOTS.  However, whereas
;;; for BUILT-IN-CLASS CLASS-DIRECT-SLOTS must return the empty list
;;; according to the MOP, this function returns the direct default
;;; slots of the built-in class.
(defgeneric direct-slots (class))

;;; This function will be removed later.  For now it plays the role of
;;; the function CL:DOCUMENTATION. 
(defgeneric c-documentation (generic-function))

;;; This function will be removed later.  For now it plays the role of
;;; the function (SETF CL:DOCUMENTATION). 
(defgeneric (setf c-documentation) (documentation generic-function))

;;; This function is used by the class finalization protocol to set
;;; the flag in the class that indicates that it is finalized.
(defgeneric (setf c-finalized-p) (new-value class))

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

;;; This function is used by the class finalization protocol to set
;;; the default initargs of the class. 
(defgeneric (setf class-default-initargs) (default-initargs class))

;;; For STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS, this function
;;; returns the same value as CLASS-SLOTS.  However, whereas for
;;; BUILT-IN-CLASS CLASS-SLOTS must return the empty list according to
;;; the MOP, this function returns the effective default slots of the
;;; built-in class.
(defgeneric effective-slots (class))

;;; This function is used by the class finalization protocol to set
;;; the effective slots of the class. 
(defgeneric (setf c-slots) (effective-slots class))

;;; This function sets the class prototype of the class.
(defgeneric (setf c-prototype) (prototype class))

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
