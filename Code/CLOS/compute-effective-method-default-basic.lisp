(cl:in-package #:sicl-clos)

;;;; This file contains an ordinary function that, given a list of
;;;; methods, computes an effective method using the standard method
;;;; combination.  This function is named
;;;; COMPUTE-EFFECTIVE-METHOD-DEFAULT.
;;;;
;;;; In this version of COMPUTE-EFFECTIVE-METHOD-DEFAULT, we do not
;;;; use the compiler to produce the effective method.  As a
;;;; consequence, the effective method will contain loops that iterate
;;;; over the different methods in each role. 
;;;;
;;;; The file compute-effective-method-default.lisp contains a version
;;;; of this function that does use the compiler, so that the
;;;; resulting effective method contains only straight-line code.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))

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
		    do (funcall (method-function method) args))))
	  (after-chain
	    (lambda (args)
	      (loop for method in after-methods
		    do (funcall (method-function method) args)))))
      (lambda (&rest args)
	(if (null around-methods)
	    (progn (funcall before-chain args)
		   (funcall primary-chain args)
		   (funcall after-chain args))
	    (funcall (method-function (car around-methods))
		     (append (cdr around-methods)
			     (list (lambda (args)
				     (funcall before-chain args)
				     (funcall primary-chain args)
				     (funcall after-chain args))))))))))
