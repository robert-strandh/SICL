(cl:in-package #:sicl-clos)

(defmacro defmethod (function-name &rest rest)
  (multiple-value-bind
	(qualifiers lambda-list specializers declarations documentation forms)
      (parse-defmethod rest)
    (let ((generic-function-var (gensym)))
      `(let* ((rt-env (load-time-value (sicl-genv:global-environment)))
	      (,generic-function-var
		(ensure-generic-function ',function-name
					 :environment rt-env)))
	 (ensure-method
	  ,generic-function-var
	  rt-env
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ,(canonicalize-specializers specializers)
	  :documentation ,documentation
	  :function
	  ,(sicl-clos::make-method-lambda-default
	    nil nil
	    `(lambda ,lambda-list
	       ,@declarations
	       ,@forms)
	    nil))))))
