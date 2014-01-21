(cl:in-package #:sicl-clos)

;;; Given a list of classes of the required arguments of a standard
;;; generic function, compute the applicable methods, independently of
;;; their qualifiers, but sorted in order from most to least specific.
;;;
;;; The applicable methods are found by filtering out methods for
;;; which every specializer is a (non-strict) subclass of the
;;; corresponding argument class.  Then they are sorted according to
;;; the order determined by METHOD-MORE-SPECIFIC-P as defined above. 
;;;
;;; We extract the body of this method into a separate function,
;;; because there is a potential metastability problem here, and that
;;; is when the function COMPUTE-APPLICABLE-METHODS-USING-CLASSES is
;;; called with the function COMPUTE-APPLICABLE-METHODS-USING-CLASSES
;;; itself as an argument.  We avoid the metastability problem by
;;; recognizing that COMPUTE-APPLICABLE-METHODS-USING-CLASSES is a
;;; standard generic function and having the discriminating function
;;; of COMPUTE-APPLICABLE-METHODS-USING-CLASSES test for the class of
;;; its argument being the class named STANDARD-GENERIC-FUNCTION.
;;; Since portable programs are not allowed to add a method to the
;;; function COMPUTE-APPLICABLE-METHODS-USING-CLASSES unless that
;;; method has a specializer other than STANDARD-GENERIC-FUNCTION or
;;; any of its superclasses, we know that in this case, only the
;;; method below is applicable, so the discriminating function will
;;; call the extracted method body directly.
;;;
;;; The special case for the discriminating function is introduced by
;;; COMPUTE-DISCRIMINATING-FUNCTION, so there is no trace of it here. 

(defun compute-applicable-methods-using-classes-default
    (standard-generic-function classes-of-arguments)
  (block b
    (values
     (sort (loop for method in (generic-function-methods generic-function)
		 when (let ((a (maybe-applicable-p method classes-of-arguments)))
			(if (eq a :somtimes)
			    (return-from b (values '() nil))
			    a))
		   collect method)
	   (lambda (method1 method2)
	     (method-more-specific-p method1 method2 classes-of-arguments)))
     t)))

(defmethod compute-applicable-methods-using-classes
    ((generic-function standard-generic-function) classes-of-arguments)
  (compute-applicable-methods-using-classes-default
   generic-function
   classes-of-arguments))
