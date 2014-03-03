(cl:in-package #:sicl-boot-phase1)

(defparameter *bridge-classes* '())

(defun add-bridge-class (name function)
  (when (member name *bridge-classes* :key #'car :test #'equal)
    (error "Attempt to define bridge class ~s twice." name))
  (push (cons name function) *bridge-classes*))

(defun find-bridge-class (name &optional (error-p t))
  (let ((entry (assoc name *bridge-classes* :test #'equal)))
    (if (null entry)
	(if error-p
	    (error "There is no bridge class named ~s." name)
	    nil)
	(cdr entry))))

(defun delete-bridge-class (name)
  (setf *bridge-classes*
	(remove name *bridge-classes* :key #'car :test #'equal)))
