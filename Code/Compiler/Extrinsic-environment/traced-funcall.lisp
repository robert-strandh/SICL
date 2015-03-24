(cl:in-package #:sicl-extrinsic-environment)

(defparameter *trace-funcall* nil)

(defparameter *depth* 0)

(defun traced-funcall (environment function &rest arguments)
  (if *trace-funcall*
      (let* ((entries (sicl-simple-environment::function-entries environment))
	     (entry (block nil
		      (maphash
		       (lambda (key entry)
			 (declare (ignore key))
			 (when (eq function
				   (car (sicl-simple-environment::function-cell entry)))
			   (return entry)))
		       entries)))
	     (name (if (null entry)
		       "???"
		       (sicl-simple-environment::name entry)))
	     (result nil))
	(loop repeat *depth*
	      do (format *trace-output* " "))
	(format *trace-output*
		"calling ~s with arguments: ~s~%" name arguments)
	(let ((*depth* (1+ *depth*)))
	  (setq result (multiple-value-list (apply function arguments))))
	(loop repeat *depth*
	      do (format *trace-output* " "))
	(format *trace-output*
		"~s returned: ~s~%" name result)
	(apply #'values result))
      (apply function arguments)))
