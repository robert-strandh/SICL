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

(defun method-more-specific-p (method1 method2 classes-of-arguments indices)
  (loop with specializers1 = (method-specializers method1)
        with specializers2 = (method-specializers method2)
        for index in indices
        for s1 = (nth index specializers1)
        for s2 = (nth index specializers2)
        for class-of-argument = (nth index classes-of-arguments)
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
(defun maybe-applicable-p (method classes profile)
  (loop with result = t
        for specializer in (method-specializers method)
        for class in classes
        for relevant-p in profile
        do (cond ((not relevant-p)
                  nil)
                 ((classp specializer)
                  (unless (subclassp class specializer)
                    (return-from maybe-applicable-p nil)))
                 ((eq (class-of (eql-specializer-object specializer)) class)
                  (setf result :sometimes))
                 (t
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

(defun precedence-indices (lambda-list precedence-order)
  (let ((required (subseq lambda-list 0 (length precedence-order))))
    (loop for parameter in precedence-order
          collect (position parameter required))))

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
  (let* ((classes-of-arguments (mapcar #'class-of arguments))
         (lambda-list (generic-function-lambda-list generic-function))
         (precedence-order (generic-function-argument-precedence-order generic-function))
         (indices (precedence-indices lambda-list precedence-order)))
    (let ((result (sort
                   (loop for method in (generic-function-methods generic-function)
                         when (definitely-applicable-p method arguments)
                           collect method)
                   (lambda (method1 method2)
                     (method-more-specific-p
                      method1
                      method2
                      classes-of-arguments
                      indices)))))
      result)))

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
  (let* ((lambda-list (generic-function-lambda-list generic-function))
         (profile (specializer-profile generic-function))
         (precedence-order (generic-function-argument-precedence-order  generic-function))
         (indices (precedence-indices lambda-list precedence-order)))
    (block b
      (values
       (let ((result (sort
                      (loop for method in (generic-function-methods generic-function)
                            when (let ((a (maybe-applicable-p method classes-of-arguments profile)))
                                   (if (eq a :sometimes)
                                       (return-from b (values '() nil))
                                       a))
                              collect method)
                      (lambda (method1 method2)
                        (method-more-specific-p
                         method1
                         method2
                         classes-of-arguments
                         indices)))))
         result)
       t))))
