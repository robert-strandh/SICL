(cl:in-package #:sicl-clos)

;;;; This definition of the macro DEFMETHOD is less general than the
;;;; normal definition.  It is only guaranteed to work when the
;;;; generic function is of type STANDARD-GENERIC-FUNCTION, and the
;;;; method to be created can be of type STANDARD-METHOD.
;;;;
;;;; The advantage of this definition is that it does not require the
;;;; full generality of MAKE-METHOD-LAMBDA.  Instead, it uses the
;;;; default version of MAKE-METHOD-LAMBDA.

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
