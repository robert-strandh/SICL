(cl:in-package #:sicl-boot-phase2)

(defparameter *find-ersatz-class*
  #'find-ersatz-class)

(defparameter *add-ersatz-class*
  #'add-ersatz-class)

(defun *ensure-class (name
		      &rest arguments
		      &key
			direct-default-initargs
			direct-slots
			direct-superclasses
			(metaclass nil metaclass-p)
		      &allow-other-keys)
  ;; If the class already exists, then do nothing.
  (let ((class (funcall *find-ersatz-class* name nil)))
    (if (null class)
	(let ((superclasses
		(loop for name in direct-superclasses
		      for class = (funcall *find-ersatz-class* name)
		      collect class))
	      (remaining-keys (copy-list arguments)))
	  (loop while (remf remaining-keys :metaclass))
	  (loop while (remf remaining-keys :direct-superclasses))
	  (let* ((class (if metaclass-p
			    metaclass
			    'standard-class))
		 (result (apply #'make-instance class
				:direct-default-initargs direct-default-initargs
				:direct-slots direct-slots
				:name name
				:direct-superclasses superclasses
				remaining-keys)))
	    (funcall *add-ersatz-class* name result)
	    result))
	class)))
