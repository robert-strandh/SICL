(cl:in-package #:sicl-clos)

;;;; This file contains the definition of global variables that will
;;;; hold information about certain classes.  This information is
;;;; computed as part of the class initialization and class
;;;; finalization protocols, and is later used to deal with
;;;; metastability problems.

;;; The value of this variable will be the class metaobject
;;; corresponding to the class named STANDARD-CLASS. 
(defvar *standard-class*)

;;; Let I be an instance such that (CLASS-OF I) is the class named
;;; STANDARD-CLASS.  Let L be the location in I of the slot that
;;; contains the value returned by a call to CLASS-SLOTS with I as an
;;; argument.  This variable will contain the location L.
;;;
;;; We use the value of this variable in SLOT-VALUE to deal with a
;;; metastability problem.  Given an instance I passed as an argument
;;; to SLOT-VALUE.  Before SLOT-VALUE can call SLOT-VALUE-USING-CLASS,
;;; it must traverse the list of effective slot definitions returned
;;; by a call to (CLASS-SLOTS (CLASS-OF I)).  However, CLASS-SLOTS is
;;; a reader function, so it will call SLOT-VALUE, resulting in an
;;; infinite recursion.  We break the call chain by recognizing the
;;; special case when (CLASS-OF I) is the class named STANDARD-CLASS,
;;; in which case we use the location stored in this variable to
;;; access the slot directly. 
(defvar *standard-class-class-slots-location*)

;;; The value of this variable will be the class metaobject
;;; corresponding to the class named
;;; STANDARD-EFFECTIVE-SLOT-DEFINITION.
(defvar *standard-effective-slot-definition*)

;;; Let I be an instance such that (CLASS-OF I) is the class named
;;; STANDARD-EFFECTIVE-SLOT-DEFINITION.  Let L be the location in I of
;;; the slot that contains the value returned by a call to
;;; SLOT-DEFINITION-NAME with I as an argument.  This variable will
;;; contain the location L.
;;;
;;; We use the value of this variable in SLOT-VALUE to deal with a
;;; metastability problem.  Given an instance I passed as an argument
;;; to SLOT-VALUE.  Before SLOT-VALUE can call SLOT-VALUE-USING-CLASS,
;;; it must traverse the list of effective slot definitions returned
;;; by a call to (CLASS-SLOTS (CLASS-OF I)).  To find the right
;;; effective slot definition, SLOT-VALUE compares the slot name that
;;; it received as an argument to the value of SLOT-DEFINITION-NAME of
;;; each of the effective slot definition metaobjects in the list.
;;; However, SLOT-DEFINITION-NAME is a reader function, so it will
;;; call SLOT-VALUE, resulting in an infinite recursion.  We break the
;;; call chain by recognizing the special case when (CLASS-OF I) is
;;; the class named STANDARD-EFFECTIVE-SLOT-DEFINITION, in which case
;;; we use the location stored in this variable to access the slot
;;; directly.
(defvar *standard-effective-slot-definition-name-location*)

;;; Let I be an instance such that (CLASS-OF I) is the class named
;;; STANDARD-EFFECTIVE-SLOT-DEFINITION.  Let L be the location in I of
;;; the slot that contains the value returned by a call to
;;; SLOT-DEFINITION-LOCATION with I as an argument.  This variable
;;; will contain the location L.
;;;
;;; We use the value of this variable in SLOT-VALUE-USING-CLASS to
;;; deal with a metastability problem.  Given an instance I, a class C
;;; which is the value of (CLASS-OF I), and an effective
;;; slot-definition S passed as arguments to SLOT-VALUE-USING-CLASS.
;;; To find the value of the slot, SLOT-VALUE-USING-CLASS must
;;; determine the location in I of the slot defined by S.  It does
;;; that by calling SLOT-DEFINITION-LOCATION with S as an argument.
;;; However, SLOT-DEFINITION-LOCATION is a reader function, so it will
;;; call SLOT-VALUE which will call SLOT-VALUE-USING-CLASS, resulting
;;; in an infinite recursion.  We break the call chain by recognizing
;;; the special case when C is the class named
;;; STANDARD-EFFECTIVE-SLOT-DEFINITION, in which case we use the
;;; location stored in this variable to access the slot directly.
(defvar *standard-effective-slot-definition-location-location*)

