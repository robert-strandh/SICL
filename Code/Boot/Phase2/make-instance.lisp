(cl:in-package #:sicl-boot-phase2)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (apply #'sicl-boot-phase1:make-instance-default
	 class
	 #'initialize-instance
	 initargs))
