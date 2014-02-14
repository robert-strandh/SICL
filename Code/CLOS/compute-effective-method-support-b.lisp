(cl:in-package #:sicl-clos)

;;;; This file contains the support code for the generic function
;;;; COMPUTE-EFFECTIVE-METHOD.
;;;;
;;;; In this file, there are no definitions of generic functions, nor
;;;; of any methods.  

;;;; This file contains an ordinary function that, given a list of
;;;; methods, computes an effective method using the standard method
;;;; combination.  This function is named
;;;; COMPUTE-EFFECTIVE-METHOD-DEFAULT.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/compute-effective-method.html
;;;
;;; The specification includes a single method, specialized only for
;;; STANDARD-GENERIC-FUNCTION, independently of the method
;;; combination.  The default actions below is valid for
;;; STANDARD-GENERIC-FUNCTION and the STANDARD method combination. 

;;; In this version of COMPUTE-EFFECTIVE-METHOD-DEFAULT, we do not
;;; use the compiler to produce the effective method.  As a
;;; consequence, the effective method will contain loops that iterate
;;; over the different methods in each role. 
;;;
;;; For a version that DOES use the compiler, see the file
;;; compute-effective-method-support-a.lisp
(defun compute-effective-method-default (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
	(before-methods (remove-if-not #'before-method-p methods))
	(after-methods (reverse (remove-if-not  #'after-method-p methods)))
	(around-methods (remove-if-not  #'around-method-p methods)))
    (when (null primary-methods)
      (error "no primary method"))
    (let ((primary-chain
	    (lambda (args)
	      (funcall (method-function (car primary-methods))
		       args
		       (cdr primary-methods))))
	  (before-chain
	    (lambda (args)
	      (loop for method in before-methods
		    do (funcall (method-function method) args '()))))
	  (after-chain
	    (lambda (args)
	      (loop for method in after-methods
		    do (funcall (method-function method) args '())))))
      (lambda (&rest args)
	(if (null around-methods)
	    (progn (funcall before-chain args)
		   (multiple-value-prog1
		       (funcall primary-chain args)
		     (funcall after-chain args)))
	    (funcall (method-function (car around-methods))
		     (append (cdr around-methods)
			     (list (lambda (args)
				     (funcall before-chain args)
				     (multiple-value-prog1
					 (funcall primary-chain args)
				       (funcall after-chain args)))))))))))
