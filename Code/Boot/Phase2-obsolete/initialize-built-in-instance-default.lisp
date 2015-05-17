(cl:in-package #:sicl-boot-phase2)

(defparameter *initialize-built-in-instance-default*
  #'sicl-boot-phase1:initialize-built-in-instance-default)

(defun initialize-built-in-instance-default (&rest args)
  (apply *initialize-built-in-instance-default* args))
