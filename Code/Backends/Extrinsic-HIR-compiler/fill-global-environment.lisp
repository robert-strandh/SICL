(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; We also need the function FUNCALL, because that is what is used by
;;; the default SETF expander.  At the momement, it only handles
;;; functions as its first argument.
(setf (sicl-env:fdefinition 'funcall *environment*)
      (lambda (&rest args)
	(unless (plusp (length args))
	  (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "wrong number of arguments"))
	(unless (functionp (first args))
	  (funcall (sicl-env:fdefinition 'cl:error *environment*)
		   "First argument to must be a function ~s"
		   (first args)))
	(apply (first args) (rest args))))

;;; We need a definition of the function VALUES, and the host one is
;;; just fine for this.
(setf (sicl-env:fdefinition 'values *environment*)
      #'values)

;;; Set the variable SICL-ENV:*ENVIRONMENT* in the environment.
(setf (sicl-env:special-variable 'sicl-env:*global-environment* *environment* t)
      *environment*)

;;; This definition allows us to find the definition of any host function.
;;; It is not ideal right now because it can fail and call ERROR.
(setf (sicl-env:fdefinition 'host-fdefinition *environment*)
      #'fdefinition)

;;; Fill the target environment with all available packages in the host.
(setf (sicl-env:packages *environment*)
      (list-all-packages))
