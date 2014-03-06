(cl:in-package #:sicl-clos)

(defparameter *target-classes* '())

(defun add-target-class (name function)
  (when (member name *target-classes* :key #'car :test #'equal)
    (error "Attempt to define target class ~s twice." name))
  (push (cons name function) *target-classes*))

(defun find-target-class (name &optional (error-p t))
  (let ((entry (assoc name *target-classes* :test #'equal)))
    (if (null entry)
	(if error-p
	    (error "There is no target class named ~s." name)
	    nil)
	(cdr entry))))

(defun delete-target-class (name)
  (setf *target-classes*
	(remove name *target-classes* :key #'car :test #'equal)))
