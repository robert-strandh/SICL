(cl:in-package #:sicl-boot-phase2)

(defparameter *shared-initialize-default*
  #'sicl-boot-phase1:shared-initialize-default)

(defun shared-initialize-default (&rest args)
  (apply *shared-initialize-default* args))
