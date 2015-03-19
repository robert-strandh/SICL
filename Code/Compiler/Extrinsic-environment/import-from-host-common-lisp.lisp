(cl:in-package #:sicl-extrinsic-environment)

(defun import-variables (environment)
  (let ((variables '(*standard-input* *standard-output* *error-output*
		     *terminal-io* *trace-output* *query-io*
		     *print-base* *read-base* *package*)))
    (loop for symbol in variables
	  do (setf (sicl-env:special-variable symbol environment t)
		   (cl:symbol-value symbol)))))

(defun import-from-host-common-lisp (environment)
  (import-variables environment))
  
