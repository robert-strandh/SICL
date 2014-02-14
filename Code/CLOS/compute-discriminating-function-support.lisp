(cl:in-package #:sicl-clos)

;;; This function can not itself be the discriminating function of a
;;; generic function, because it also takes the generic function
;;; itself as an argument.  However it can be called by the
;;; discriminating function, in which case the discriminating function
;;; must supply the GENERIC-FUNCTION argument either from a
;;; closed-over variable, from a compiled-in constant, or perhaps by
;;; some other mechanism.
(defun default-discriminating-function (generic-function arguments)
  (let ((applicable-methods
	  (compute-applicable-methods generic-function arguments)))
    (when (null applicable-methods)
      (apply #'no-applicable-method generic-function arguments))
    (let* ((method-combination
	     (generic-function-method-combination generic-function))
	   (effective-method
	     (compute-effective-method generic-function
				       method-combination
				       applicable-methods)))
      (apply effective-method arguments))))
