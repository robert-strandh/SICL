(cl:in-package #:sicl-clos)

;;; This function takes a generic function an returns a default
;;; discriminating function for it.
(defun make-default-discriminating-function (generic-function)
  (lambda (&rest arguments)
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
	(apply effective-method arguments)))))

(defun compute-discriminating-function-default (generic-function)
  (make-default-discriminating-function generic-function))
