(cl:in-package #:sicl-boot-phase1)

(defun class-of (object)
  (cond ((heap-instance-p object)
	 (heap-instance-class object))
	((typep object 'cl:standard-object)
	 (cl:class-of object))
	(t
	 *t*)))

