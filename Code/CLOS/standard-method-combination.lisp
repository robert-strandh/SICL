(cl:in-package #:sicl-clos)

(define-method-combination standard ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
	   (loop for method in methods
		 collect `(call-method ,method))))
    (let ((form (if (and (null before)
			 (null after)
			 (null (rest primary)))
		    `(call-method ,(first primary))
		    `(multiple-value-prog1
			 (progn ,@(call-methods before)
				(call-method ,(first primary)
					     ,(rest primary)))
		       ,@(call-methods (reverse after))))))
      (if (null around)
	  form
	  `(call-method ,(first around)
			(,@(rest around)
			 (make-method ,form)))))))
