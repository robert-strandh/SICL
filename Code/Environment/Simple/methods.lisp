(cl:in-package #:sicl-simple-environment)

(defmethod sicl-env:fboundp (function-name (env simple-environment))
  (let ((entry (find function-name (function-entries env)
		     :key #'name :test #'equal)))
    (and (not (null entry))
	 (not (eq (car (function-cell entry))
		  (unbound entry))))))
