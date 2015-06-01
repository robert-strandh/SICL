(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SICL-specific accessors.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for specializer metaobjects other than classes.

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

;;; For a slot with :ALLOCATION :CLASS, this function returns a CONS
;;; cell where the CAR is used to store the value of the slot.
(defgeneric slot-definition-storage (direct-slot-definition))

(defgeneric (setf slot-definition) (slot-definition accessor-method))
