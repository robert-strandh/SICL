(cl:in-package #:sicl-boot-phase2)

(defparameter *make-instance-default*
  #'sicl-boot-phase1:make-instance-default)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (find-bridge-class class)))
  (apply *make-instance-default*
	 class
	 #'initialize-instance
	 initargs))
