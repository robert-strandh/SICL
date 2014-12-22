(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; This is a temporary definition of CL:TYPEP.
(defun typep (object type-specifier &optional environment)
  (let ((env (if (null environment)
		 sicl-env:*global-environment*
		 (cleavir-env:global-environment environment))))
    (labels ((aux (type-specifier)
	       (cond ((consp type-specifier)
		      (cond ((eq (first type-specifier) 'and)
			     (every #'aux (rest type-specifier)))
			    ((eq (first type-specifier) 'or)
			     (some #'aux (rest type-specifier)))
			    ((eq (first type-specifier) 'not)
			     (not (aux (second type-specifier))))
			    (t
			     (let ((expander (sicl-env:type-expander
					      (first type-specifier)
					      env)))
			       (if (null expander)
				   (host-cl:typep object type-specifier)
				   (aux (funcall expander
						 type-specifier
						 env)))))))
		     ((symbolp type-specifier)
		      (let ((expander (sicl-env:type-expander
				       type-specifier
				       env)))
			(if (null expander)
			    (host-cl:typep object type-specifier)
			    (aux (funcall expander
					  (list type-specifier)
					  env)))))
		     (t
		      (host-cl:typep object type-specifier)))))
      (aux type-specifier))))
		      
		      
					    
  
