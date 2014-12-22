(cl:in-package #:sicl-extrinsic-hir-compiler)

;;; This is a temporary definition of CL:ENSURE-GENERIC-FUNCTION.  It
;;; only processes the LAMBDA-LIST and the ENVIRONMENT keyword
;;; arguments, and it is not able to redefine existing
;;; generic-functions.  If there is already a function with the name
;;; NAME, then that generic function is returned.  Otherwise, a
;;; generic function is created and associated with NAME in the
;;; environment.

(defun ensure-generic-function
    (name &key lambda-list environment &allow-other-keys)
  (if (sicl-env:fboundp name environment)
      (sicl-env:fdefinition name environment)
      (let ((result (make-instance
			(sicl-env:find-class 'standard-generic-function
					     environment)
		      :name name
		      :lambda-list lambda-list)))
	(setf (sicl-env:fdefinition name environment)
	      result)
	result)))
