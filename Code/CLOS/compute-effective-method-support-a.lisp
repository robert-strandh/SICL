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

;;; In this version of the default action, we use the compiler to
;;; produce the effective method so that the effective method
;;; contains only straight-line code and no loops.
;;;
;;; For a version that does not use the compiler, see the file
;;; compute-effective-method-support-b.lisp
(defun compute-effective-method-default (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
	(before-methods (remove-if-not #'before-method-p methods))
	(after-methods (remove-if-not  #'after-method-p methods))
	(around-methods (remove-if-not  #'around-method-p methods)))
    (when (null primary-methods)
      (error "no primary method"))
    (let ((primary-chain
	    `(funcall ,(method-function (car primary-methods))
		      args
		      '(,@(loop for method in (cdr primary-methods)
				collect (method-function method)))))
	  (before-chain
	    (loop for method in before-methods
		  collect `(funcall ,(method-function method)
				    args
				    '())))
	  (after-chain
	    (loop for method in (reverse after-methods)
		  collect `(funcall ,(method-function method)
				    args
				    '()))))
      (compile
       nil
       (if (null around-methods)
	   `(lambda (&rest args)
	      ,@before-chain
	      (multiple-value-prog1
		  ,primary-chain
		,@after-chain))
	   `(lambda (&rest args)
	      (funcall ,(method-function (car around-methods))
		       args
		       (list ,@(loop for method in (cdr around-methods)
				     collect (method-function method))
			     (lambda (args next-methods)
			       (declare (ignore next-methods))
			       ,@before-chain
			       (multiple-value-prog1
				   ,primary-chain
				 ,@after-chain))))))))))
