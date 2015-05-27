(cl:in-package #:sicl-boot)

(defun message (control-string &rest args)
  (apply #'format *trace-output* control-string args)
  (finish-output *trace-output*))
