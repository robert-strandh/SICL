(cl:in-package #:sicl-clos)

;;;; This file contains an ordinary function that, given a list of
;;;; methods, computes an effective method using the standard method
;;;; combination.  This function is named
;;;; COMPUTE-EFFECTIVE-METHOD-DEFAULT.
;;;;
;;;; In this version of COMPUTE-EFFECTIVE-METHOD-DEFAULT, we use the
;;;; compiler to produce the effective method so that the effective
;;;; method contains only straight-line code and no loops.
;;;;
;;;; The file compute-effective-method-default-basic.lisp contains a
;;;; version of this function that does not use the compiler.

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
	      ,primary-chain
	      ,@after-chain)
	   `(lambda (&rest args)
	      (funcall ,(method-function (car around-methods))
		       args
		       (list ,@(loop for method in (cdr around-methods)
				     collect (method-function method))
			     (lambda (args next-methods)
			       (declare (ignore next-methods))
			       ,@before-chain
			       ,primary-chain
			       ,@after-chain)))))))))
	     
