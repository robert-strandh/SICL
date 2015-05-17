(cl:in-package #:sicl-boot-phase2)

(defparameter *make-instance-default*
  #'sicl-boot-phase1:make-instance-default)

(defparameter *find-class*
  #'find-bridge-class)

(defun make-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (funcall *find-class* class)))
  (apply *make-instance-default*
	 class
	 #'initialize-instance
	 initargs))
