(cl:in-package #:sicl-clos)

;;; At the moment, this macro does not have any compile-time side
;;; effects.

;;; CT-ENV is the compile-time environment in which the call to
;;; DEFMETHOD is compiled.  We check the compile-time environment to
;;; see whether there is a generic function named FUNCTION-NAME in it.
;;; If there is such a generic function, it is used as the first
;;; argument to MAKE-METHOD-LAMBDA, and the CLASS-PROTOTYPE of the
;;; GENERIC-FUNCTION-METHOD-CLASS of that function is used as the
;;; second argument to MAKE-METHOD-LAMBDA.  If no such generic
;;; function is found, we use the CLASS-PROTOTYPE of the class named
;;; STANDARD-GENERIC-FUNCTION in CT-ENV as the first argument to
;;; MAKE-METHOD-LAMBDA, and we use the CLASS-PROTOTYPE of the class
;;; named STANDARD-METHOD in CT-ENV as the second argument to
;;; MAKE-METHOD-LAMBDA.
;;;
;;; The expansion of this macro calls the SICL-specific function
;;; ENSURE-METHOD.  The first argument to ENSURE-METHOD is the generic
;;; function named FUNCTION-NAME in the run-time environment (and not
;;; in the compile-time environment) in which the expansion is
;;; evaluated.  The second argument to ENSURE-METHOD is the run-time
;;; environment itself.  The function ENSURE-METHOD uses the
;;; environment argument to take symbols naming specializers and to
;;; look up the corresponding classes.

(defmacro defmethod (&environment ct-env function-name &rest rest)
  (multiple-value-bind
	(qualifiers lambda-list specializers declarations documentation forms)
      (parse-defmethod rest)
    (let* ((fboundp (sicl-genv:fboundp function-name ct-env))
	   (binding (if fboundp
			(sicl-genv:fdefinition function-name ct-env)
			nil))
	   (fun (if (and binding (typep binding 'generic-function))
		    binding
		    nil))
	   (generic-function-var (gensym)))
      `(let* ((rt-env (load-time-value (sicl-genv:global-environment)))
	      (,generic-function-var
		(ensure-generic-function ',function-name :environment rt-env)))
	 (ensure-method
	  ,generic-function-var
	  rt-env
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ,(canonicalize-specializers specializers)
	  :documentation ,documentation
	  :function
	  ,(make-method-lambda
	    (if (null fun)
		(class-prototype
		 (sicl-environment:find-class 'standard-generic-function ct-env))
		fun)
	    (class-prototype
	     (if (null fun)
		 (sicl-environment:find-class 'standard-method ct-env)
		 (generic-function-method-class fun)))
	    `(lambda ,lambda-list
	       ,@declarations
	       ,@forms)
	    nil))))))
