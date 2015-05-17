(cl:in-package #:sicl-clos)

(defparameter *ersatz-generic-functions* '())

(defun add-ersatz-generic-function (name function)
  (pushnew (cons name function) *ersatz-generic-functions*
	   :key #'car :test #'equal))

(defun find-ersatz-generic-function (name &optional (error-p t))
  (let ((entry (assoc name *ersatz-generic-functions* :test #'equal)))
    (if (null entry)
	(if error-p
	    (error "There is no ersatz generic function named ~s." name)
	    nil)
	(cdr entry))))

(defun delete-ersatz-generic-function (name)
  (setf *ersatz-generic-functions*
	(remove name *ersatz-generic-functions* :key #'car :test #'equal)))
