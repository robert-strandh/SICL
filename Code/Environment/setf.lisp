(in-package #:sicl-global-environment)

(defmacro setf (place new-value-form &rest more-pairs)
  (cond ((null more-pairs)
	 (multiple-value-bind (variables
			       values
			       store-variables
			       writer-form
			       reader-form)
	     (get-setf-expansion place)
	   (declare (ignore reader-form))
	   `(let* ,(mapcar #'list variables values)
	      (multiple-value-bind ,store-variables
		  ,new-value-form
		,writer-form))))
	((not (null (cdr more-pairs)))
	 `(progn (setf ,place ,new-value-form)
		 (setf ,@more-pairs)))
	(t
	 (error "Odd number of arguments to SETF."))))

		
		
