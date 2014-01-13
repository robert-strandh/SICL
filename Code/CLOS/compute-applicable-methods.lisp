(cl:in-package #:sicl-clos)

;;;; This file contains definitions of ordinary auxiliary functions
;;;; used by COMPUTE-APPLICABLE-METHODS-USING-CLASSES.

;;; Class C1 is a sub-specizlizer of class C2 with respect to some
;;; argument class C if and only if C1 occurs before C2 in the class
;;; precedence list of C.
;;;
;;; Recall that this function is used to determine whether one
;;; applicable method is more specific than another applicable method.
;;; Thus, we have already determined that C is more specific than both
;;; C1 and C2, and therefore both C1 and C2 are in the class
;;; precedence list of C,
;;;
;;; Should we ever be called with classes C1 and C2 that are not in
;;; the class precedence list of C, then the method we use (numeric
;;; comparison of the result of calling POSITION), will signal an
;;; error, which is reassuring.
(defun sub-specializer-p (class1 class2 class-of-argument)
  (let ((precedence-list (class-precedence-list class-of-argument)))
    (< (position class1 precedence-list) (position class2 precedence-list))))

;;; Determine whether a method is more specific than another method
;;; with respect to a list of classes of required arguments.  
;;;
;;; Recall that whether a method is more or less specific than another
;;; method is also a function of the classes of the arguments, because
;;; the order of two classes in the class precedence list of two
;;; different argument classes can be different.  
;;;
;;; This function is called only with applicable methods with respect
;;; to the classes of the arguments supplied.  
;;;
;;; It is possible for two methods of a generic function to be equally
;;; specific (which then means that they have the same specializer in
;;; every required position), but then they must have different
;;; qualifiers.  This function is called with all applicable
;;; functions, independent of the qualifiers, so this situation might
;;; happen here.
;;;
;;; FIXME: take into account the argument precedence order.
(defun method-more-specific-p (method1 method2 classes-of-arguments)
  (loop for s1 in (method-specializers method1)
	for s2 in (method-specializers method2)
	for class-of-argument in classes-of-arguments
	unless (eq s1 s2)
	  return (sub-specializer-p s1 s2 class-of-argument)))

;;; Determine whether a class C1 is a subclass of another class C2.
;;; This can be done by checking whether C2 is in the class precedence
;;; list of C1.  The relation is not strict, so that we return true if
;;; the two are the same.
(defun subclassp (class1 class2)
  (member class2 (class-precedence-list class1)))

;;; Determine whether a method is applicable to a sequence of argument
;;; classes.  The result can be either T or NIL or :SOMETIMES.  The
;;; result is :SOMETIMES when there is an EQL specializer with an
;;; object whose class is identical to the corresponding argument
;;; class, because if and only if this holds, the argument may or may
;;; not be the one that is specialized for.
(defun method-applicable-p (method classes)
  (loop with result = t
	for specializer in (method-specializers method)
	for class in classes
	do (if (classp specializer)
	       (unless (subclassp class specializer)
		 (return-from method-applicable-p nil))
	       (when (eq (class-of (eql-specializer-object specializer)) class)
		 (setf result :sometimes)))
	finally (return result)))
