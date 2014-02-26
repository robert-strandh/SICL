(cl:in-package #:sicl-clos)

(defun add-direct-subclass-default (superclass subclass)
  (when (member subclass (class-direct-subclasses superclass))
    (error "Attempt to add existing subclass ~s as a subclass of ~s."
	   subclass superclass))
  (setf (c-direct-subclasses superclass)
	(cons subclass (class-direct-subclasses superclass))))

(defun remove-direct-subclass-default (superclass subclass)
  (setf (c-direct-subclasses superclass)
	(remove subclass (class-direct-subclasses superclass)
		:test #'eq)))
