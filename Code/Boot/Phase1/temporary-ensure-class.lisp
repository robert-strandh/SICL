(cl:in-package #:sicl-clos)

(defun ensure-class (name
		     &rest arguments
		     &key direct-superclasses
		     &allow-other-keys)
  (let* ((generated-name (gensym))
	 (environment (sicl-genv:global-environment))
	 (new-arguments (copy-list arguments))
	 (new-superclasses
	   (loop for superclass-name in (remove t direct-superclasses)
		 for class = (sicl-genv:find-class superclass-name environment)
		 collect (class-name class))))
    (loop while (remf new-arguments :direct-superclasses))
    (let ((class (apply #'closer-mop:ensure-class
			generated-name
			:direct-superclasses new-superclasses
			new-arguments)))
      (setf (sicl-genv:find-class name environment) class)
      class)))
