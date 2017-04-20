(cl:in-package #:sicl-cons)

;;; FIXME: Handle errors better.
(defmacro remf (&environment env place indicator)
  (multiple-value-bind (vars vals store-vars writer-form reader-form)
      (get-setf-expansion place env)
    (let ((indicator-var (gensym)))
      `(let* (,@(if (null vars)
		    `()
		    `((,(first vars) ,(first vals))))
	      (,(first store-vars) ,reader-form)
	      (,indicator-var ,indicator))
       (cond ((atom ,(first store-vars))
	      (if (null ,(first store-vars))
		  nil
		  (error 'must-be-plist :expected-type store-vars)))
	     ((eq (car ,(first store-vars)) ,indicator-var)
	      (cond ((atom (cdr ,(first store-vars)))
		     (error 'must-be-plist :expected-type store-vars))
		    (t
		     (setq ,(first store-vars)
			   (cddr ,(first store-vars)))
		     ,writer-form
		     t)))
	     (t
	      (loop for rest = (cdr ,(first store-vars)) then (cddr rest)
		    until (atom (cdr rest))
		    do (cond ((atom (cddr rest))
			      (error 'must-be-plist :expected-type store-vars))
			     ((eq (cadr rest) ,indicator-var)
			      (setf (cdr rest) (cdddr rest))
			      (return t))
			     (t
			      nil))
		    finally (unless (null (cdr rest))
			      (error 'must-be-plist :expected-type store-vars)))))))))
