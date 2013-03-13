(defun wrap-methods (methods)
  (if (null (cdr methods))
      `(let ((*call-next-method*
	       (lambda (&rest new-args)
		 (declare (ignore new-args))
		 (error "no next method")))
	     (*next-method-p* (lambda () nil))
	     (args (or new-args args)))
	 (apply ,(method-function (car methods)) args))
      `(let ((*call-next-method*
	       (lambda (&rest new-args)
		 ,(wrap-methods (cdr methods))))
	     (*next-method-p* (lambda () t))
	     (args (or new-args args)))
	 (apply ,(method-function (car methods)) args))))

(defun compute-effective-method-function (methods)
  (let ((primary-methods (remove-if-not #'primary-method-p methods))
	(before-methods (remove-if-not #'before-method-p methods))
	(after-methods (remove-if-not  #'after-method-p methods))
	(around-methods (remove-if-not  #'around-method-p methods)))
    (if (and (null before-methods)
	     (null after-methods)
	     (= (length primary-methods) 1))
	(car primary-methods)
	(compile nil
		 `(lambda (&rest args)
		    ,@(loop for method in before-methods
			    collect `(apply ,(method-function method) args))
		    (multiple-value-prog1
			(let ((new-args '()))
			  ,(wrap-methods primary-methods))
		      ,@(loop for method in (reverse after-methods)
			      collect `(apply ,(method-function method) args))))))))
