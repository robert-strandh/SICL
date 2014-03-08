(cl:in-package #:sicl-boot-phase3)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (sicl-boot-phase2:find-target-class class)))
  (apply #'sicl-boot-phase2:make-instance-default
	 class
	 #'initialize-instance
	 initargs))
