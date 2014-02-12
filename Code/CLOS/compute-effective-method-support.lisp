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
;;;;
;;;; The file compute-effective-method-default-basic.lisp contains a
;;;; version of this function that does not use the compiler.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Support for generic function
;;; COMPUTE-EFFECTIVE-METHOD.
;;;
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
(defun compute-effective-method-default-1 (methods)
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
	     
;;; In this version of COMPUTE-EFFECTIVE-METHOD-DEFAULT, we do not
;;; use the compiler to produce the effective method.  As a
;;; consequence, the effective method will contain loops that iterate
;;; over the different methods in each role. 
(defun compute-effective-method-default-2 (methods)
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
		    do (funcall (method-function method) args))))
	  (after-chain
	    (lambda (args)
	      (loop for method in after-methods
		    do (funcall (method-function method) args)))))
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
