(cl:in-package #:sicl-clos)

;;; Given a list of classes of the required arguments of a generic
;;; function, compute the applicable methods, independently of their
;;; qualifiers, but sorted in order from most to least specific. 
;;;
;;; The applicable methods are found by filtering out methods for
;;; which every specializer is a (non-strict) subclass of the
;;; corresponding argument class.  Then they are sorted according to
;;; the order determined by METHOD-MORE-SPECIFIC-P as defined above. 

(defmethod compute-applicable-methods-using-classes
    ((generic-function standard-generic-function) classes-of-arguments)
  (values
   (sort (loop for method in (generic-function-methods generic-function)
	       when (let ((a (method-applicable-p method classes-of-arguments)))
		      (if (eq a :somtimes)
			  (return-from compute-applicable-methods-using-classes
			    (values '() nil))
			 a))
		 collect method)
	 (lambda (method1 method2)
	   (method-more-specific-p method1 method2 classes-of-arguments)))
   t))
