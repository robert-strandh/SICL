(cl:in-package #:sicl-clos)

;;;; This file contains the support code for the generic functions
;;;; COMPUTE-APPLICABLE-METHODS and
;;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES.
;;;;
;;;; In this file, there are no definitions of generic functions, nor
;;;; of any methods.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

;;; Given two specializers S1 and S2 that are not EQ, return true if
;;; and only if S1 is a sub-specializer S2.
;;;
;;; Recall that this function is used to determine whether one
;;; applicable method is more specific than another applicable method.
;;; It follows that S1 and S2 can not both be EQL specializers,
;;; because they would then either be EQ, and this function would not
;;; be called, or they would not be EQ and one of the methods would
;;; not be applicable.
;;;
;;; If S1 is an EQL specializer, then S1 is a sub-specializer of S2.
;;; If S2 is an EQL specializer, then S2 is not a sub-specializer of
;;; S2.  Otherwise, S1 and S2 are both classes.  Class S1 is a
;;; sub-specizlizer of class S2 with respect to some argument class C
;;; if and only if S1 occurs before S2 in the class precedence list of
;;; C.
;;;
;;; If both S1 and S2 are classes, since both S1 and S2 belong to
;;; applicable methods, we have already determined that C is more
;;; specific than both S1 and S2, and therefore both S1 and S2 are in
;;; the class precedence list of C,
;;;
;;; Should we ever be called with classes S1 and S2 that are not in
;;; the class precedence list of C, then the method we use (numeric
;;; comparison of the result of calling POSITION) will signal an
;;; error, which is reassuring.
(defun sub-specializer-p (specializer1 specializer2 class-of-argument)
  (cond ((typep specializer1 'eql-specializer)
	 t)
	((typep specializer2 'eql-specializer)
	 nil)
	(t (let ((precedence-list (class-precedence-list class-of-argument)))
	     (< (position specializer1 precedence-list)
		(position specializer2 precedence-list))))))

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

;;; Determine whether a class C1 is a (not necessarily strict)
;;; subclass of another class C2.  This can be done by checking
;;; whether C2 is in the class precedence list of C1.  Since the
;;; relation is not strict, we return true if the two are the same.
;;;
;;; We use MEMBER (rather than (say) FIND) because MEMBER is a rather
;;; simple function that works only on lists, whereas we might want to
;;; make FIND a generic function.
(defun subclassp (class1 class2)
  (member class2 (class-precedence-list class1) :test #'eq))

;;; Determine whether a method is applicable to a sequence of argument
;;; classes.  The result can be either T or NIL or :SOMETIMES.  The
;;; result is :SOMETIMES when the method has at least one EQL
;;; specializer, and for each EQL specializer, the class of the
;;; underlying object is identical to the corresponding argument
;;; class.
(defun maybe-applicable-p (method classes)
  (loop with result = t
	for specializer in (method-specializers method)
	for class in classes
	do (if (classp specializer)
	       (unless (subclassp class specializer)
		 (return-from maybe-applicable-p nil))
	       (if (eq (class-of (eql-specializer-object specializer)) class)
		   (setf result :sometimes)
		   (return-from maybe-applicable-p nil)))
	finally (return result)))

;;; Determine whether a method is applicable to a sequence of
;;; arguments.  The list of arguments may contain more elements than
;;; there are required parameters, and in that case the remaining
;;; elements of the list of arguments are simply ignored.
(defun definitely-applicable-p (method arguments)
  (loop for specializer in (method-specializers method)
	for argument in arguments
	do (if (classp specializer)
	       (unless (subclassp (class-of argument) specializer)
		 (return-from definitely-applicable-p nil))
	       (unless (eql (eql-specializer-object specializer) argument)
		 (return-from definitely-applicable-p nil)))
	finally (return t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for generic function COMPUTE-APPLICABLE-METHODS.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for STANDARD-GENERIC-FUNCTION.  The default
;;; action below is valid for that method.

(defun compute-applicable-methods-default (generic-function arguments)
  (let ((classes-of-arguments (mapcar #'class-of arguments)))
    (sort-list
     (loop for method in (generic-function-methods generic-function)
	   when (definitely-applicable-p method arguments)
	     collect method)
     (lambda (method1 method2)
       (method-more-specific-p method1 method2 classes-of-arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for generic function
;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-applicable-methods-using-classes.html
;;;
;;; The specification includes a single method on this generic
;;; function, specialized for STANDARD-GENERIC-FUNCTION.  The default
;;; action below is valid for that method.

(defun compute-applicable-methods-using-classes-default
    (generic-function classes-of-arguments)
  (block b
    (values
     (sort-list
      (loop for method in (generic-function-methods generic-function)
	    when (let ((a (maybe-applicable-p method classes-of-arguments)))
		   (if (eq a :somtimes)
		       (return-from b (values '() nil))
		       a))
	      collect method)
      (lambda (method1 method2)
	(method-more-specific-p method1 method2 classes-of-arguments)))
     t)))
