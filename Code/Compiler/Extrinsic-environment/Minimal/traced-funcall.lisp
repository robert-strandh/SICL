(cl:in-package #:sicl-minimal-extrinsic-environment)

(defparameter *trace-funcall* nil)

(defparameter *depth* 0)

(defun traced-funcall (environment source function &rest arguments)
  (if *trace-funcall*
      (let ((name (or (car (sicl-genv:function-names function environment))
		      "???"))
	    (result nil))
	(loop repeat *depth*
	      do (format *trace-output* " "))
	(format *trace-output*
		"calling ~s with arguments: ~s~%" name arguments)
	(format *trace-output* "source ~s~%" source)
	(let ((*depth* (1+ *depth*)))
	  (setq result (multiple-value-list (apply function arguments))))
	(loop repeat *depth*
	      do (format *trace-output* " "))
	(format *trace-output*
		"~s returned: ~s~%" name result)
	(apply #'values result))
      (apply function arguments)))
