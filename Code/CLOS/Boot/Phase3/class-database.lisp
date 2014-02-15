(cl:in-package #:sicl-clos)

(defparameter *target-classes* '())

(defun add-target-class (name function)
  (when (member name *target-classes* :key #'car :test #'equal)
    (error "Attempt to define target generic function ~s twice." name))
  (push (cons name function) *target-classes*))

(defun find-target-class (name)
  (let ((entry (assoc name *target-classes* :test #'equal)))
    (when (null entry)
      (error "There is no target generic function named ~s." name))
    (cdr entry)))

(defun delete-target-class (name)
  (setf *target-classes*
	(remove name *target-classes* :key #'car :test #'equal)))
