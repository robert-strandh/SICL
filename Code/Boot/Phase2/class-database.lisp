(cl:in-package #:sicl-clos)

(defparameter *ersatz-classes* '())

(defun add-ersatz-class (name function)
  (when (member name *ersatz-classes* :key #'car :test #'equal)
    (error "Attempt to define ersatz class ~s twice." name))
  (push (cons name function) *ersatz-classes*))

(defun find-ersatz-class (name &optional (error-p t))
  (let ((entry (assoc name *ersatz-classes* :test #'equal)))
    (if (null entry)
	(if error-p
	    (error "There is no ersatz class named ~s." name)
	    nil)
	(cdr entry))))

(defun delete-ersatz-class (name)
  (setf *ersatz-classes*
	(remove name *ersatz-classes* :key #'car :test #'equal)))
