(cl:in-package #:sicl-clos)

;;; Given a list of arguments of a standard generic function, compute
;;; the applicable methods, independently of their qualifiers, but
;;; sorted in order from most to least specific.
;;;
;;; We extract the body of this method into a separate function,
;;; because there is a potential metastability problem here, and that
;;; is when the function COMPUTE-APPLICABLE-METHODS is called with the
;;; function COMPUTE-APPLICABLE-METHODS itself as an argument.  We
;;; avoid the metastability problem by recognizing that
;;; COMPUTE-APPLICABLE-METHODS is a standard generic function and
;;; having the discriminating function of COMPUTE-APPLICABLE-METHODS
;;; test for the class of its argument being the class named
;;; STANDARD-GENERIC-FUNCTION.  Since portable programs are not
;;; allowed to add a method to the function COMPUTE-APPLICABLE-METHODS
;;; unless that method has a specializer other than
;;; STANDARD-GENERIC-FUNCTION or any of its superclasses, we know that
;;; in this case, only the method below is applicable, so the
;;; discriminating function will call the extracted method body
;;; directly.
;;;
;;; The special case for the discriminating function is introduced by
;;; COMPUTE-DISCRIMINATING-FUNCTION, so there is no trace of it here. 

(defun compute-applicable-methods-default (standard-generic-function arguments)
  (let ((classes-of-arguments (mapcar #'class-of arguments)))
    (sort (loop for method in (generic-function-methods generic-function)
		when (definitely-applicable-p method arguments)
		  collect method)
	  (lambda (method1 method2)
	    (method-more-specific-p method1 method2 classes-of-arguments))))

(defmethod compute-applicable-methods
    ((generic-function standard-generic-function) arguments)
  (compute-applicable-methods-default generic-function arguments))
