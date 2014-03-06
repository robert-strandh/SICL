(cl:in-package #:sicl-boot-phase2)

(defun make-built-in-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (apply #'sicl-boot-phase1:make-built-in-instance-default
	 class
	 #'initialize-built-in-instance
	 initargs))
