(cl:in-package #:sicl-extrinsic-environment)

;;; Enter every Common Lisp class into the environment.
(defun import-classes (environment)
  (loop for symbol being each external-symbol in '#:common-lisp
	for class = (find-class symbol nil)
	unless (null class)
	  do (setf (sicl-env:find-class symbol environment)
		   class)))

(defun import-variables (environment)
  (let ((variables '(*standard-input* *standard-output* *error-output*
		     *terminal-io* *trace-output* *query-io*
		     *print-base* *read-base* *package*)))
    (loop for symbol in variables
	  do (setf (sicl-env:special-variable symbol environment t)
		   (cl:symbol-value symbol)))))

(defun import-from-host-common-lisp (environment)
  (import-classes environment)
  (import-variables environment))
  
