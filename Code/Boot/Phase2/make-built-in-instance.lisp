(cl:in-package #:sicl-boot-phase2)

(defparameter *make-built-in-instance-default*
  #'sicl-boot-phase1:make-built-in-instance-default)

(defun make-built-in-instance (class &rest initargs)
  (when (symbolp class)
    (setf class (funcall *find-class* class)))
  (apply *make-built-in-instance-default*
	 class
	 #'initialize-built-in-instance
	 initargs))
