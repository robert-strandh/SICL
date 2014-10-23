(cl:in-package #:sicl-simple-environment)

;;; Recall that this function should return true if FUNCTION-NAME has
;;; a definition in ENVIRONMENT as an ordinary function, a generic
;;; function, a macro, or a special operator.
(defmethod sicl-env:fboundp (function-name (env simple-environment))
  (let ((entry (find-function-entry env function-name)))
    (and (not (null entry))
	 (or (not (eq (car (function-cell entry))
		      (unbound entry)))
	     (not (null (macro-function entry)))
	     (not (null (special-operator entry)))))))
