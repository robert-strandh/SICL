(cl:in-package #:sicl-clos)

;;; Generic function COMPUTE-APPLICABLE-METHODS-USING-CLASSES.
;;;
;;; This function is called by COMPUTE-DISCRIMINATING-FUNCTION in an
;;; attempt to determine whether the list of applicable methods can be
;;; computed using only the class of each required argument.
;;; Sometimes it is not possible to determine the list of applicable
;;; methods using only the classes of the required arguments.  This is
;;; the case if there is a method with one or more EQL specializers
;;; such that the class of the object underlying each EQL specializer
;;; is the same as the class of the argument in the corresponding
;;; position.  In other words, that method would be applicable for
;;; some instances of the list of classes of the arguments, but not
;;; for other instances.  In this situation it is therefore not enough
;;; to use the classes of the arguments to determine the list of
;;; applicable methods.
;;;
;;; This function returns two values.  If the list of applicable
;;; methods CAN be computed using only the class of each required
;;; argument, then this it returns the list of applicable methods as
;;; its first value, and TRUE as its second value.  Otherwise, it
;;; returns some arbitrary object as its first value, and FALSE as its
;;; second value.
;;;
;;; The methods in the list of applicable methods returned in the
;;; first case are ordered from more specific to less specific,
;;; independently of any qualifiers of those methods.  Notice that two
;;; methods can be equally specific, and in that case, it is not
;;; specified whether one comes before the other.  However, two
;;; methods that are equally specific must have different qualifiers,
;;; and the caller of this function separates methods with different
;;; qualifiers anyway, so the internal order does not matter in this
;;; case.

