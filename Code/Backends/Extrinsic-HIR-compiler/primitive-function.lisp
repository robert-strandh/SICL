(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; This macro allows us to import simple functions from the host
;;; environment to the target environment.  The functions are simple in
;;; that they can only have required arguments.  
;;;
;;; ARGUMENT-TYPES is a list of type specifiers.
;;;
;;; The target function that gets created takes any number of
;;; arguments of any type.  Before the host function is called with
;;; the same arguments as the ones supplied to the target function,
;;; the number of arguments and the type of each argument is checked.
;;; If there is a problem, the function CL:ERROR in the target
;;; environment is called.
(defmacro defprimitive (name argument-types)
  `(setf (sicl-env:fdefinition ',name *environment*)
	 (lambda (&rest arguments)
	   (let ((error (sicl-env:fdefinition 'cl:error *environment*)))
	     (unless (= (length arguments) ,(length argument-types))
	       (funcall error
			"wrong number of arguments ~s ~s"
			',name
			arguments))
	     ,@(loop for type in argument-types
		     for i from 0
		     collect `(unless (typep (nth ,i arguments) ',type)
				(funcall error
					 "wrong type argument ~s ~s"
					 (nth ,i arguments)
					 ',type)))
	     (apply #',name arguments)))))

					      
	    
