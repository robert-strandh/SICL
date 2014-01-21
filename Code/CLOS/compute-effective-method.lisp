(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD-FUNCTION.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))

(defun compute-effective-method-function (methods)
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
	     
