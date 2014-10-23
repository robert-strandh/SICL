(cl:in-package #:sicl-simple-environment)

(defmethod sicl-env:fboundp (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (and (not (null entry))
	 (not (eq (car (function-cell entry))
		  (unbound entry))))))
