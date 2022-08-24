(cl:in-package #:sicl-compiler)

;;; A FUNCTION DESCRIPTOR is a standard object that describes all
;;; functions that share the same code, but that may have different
;;; static environments.  Preceding the entry point in the code vector
;;; of a code object is a word that contains a reference to the
;;; function descriptor that describes the functions using that entry
;;; point.  So from a function object, in order to find the function
;;; descriptor, the entry point must be used as an address, from which
;;; 8 is subtracted in order to get the address of the word containing
;;; the reference to the function descriptor.
;;;
;;; Right now, the function descriptor contains only a few slots.
;;; Later, we intend to add information used by the call-site manager,
;;; such as the information about non-argument-parsing entry points
;;; including the location of each argument.

(defclass function-descriptor ()
  (;;; This slot contains the origin of the function, in the form of
   ;;; two source positions, representing the start and the end of the
   ;;; function code in the source.
   (%origin :initarg :origin :reader origin)
   ;;; This slot contains the code object that represents the
   ;;; compilation unit that this function is part of.
   (%compilation-unit :initarg :compilation-unit :reader compilation-unit)
   ;;; This code contains the lambda list of the function.
   (%lambda-list :initarg :lambda-list :reader lambda-list)))
