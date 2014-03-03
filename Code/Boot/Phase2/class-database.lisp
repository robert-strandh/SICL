(cl:in-package #:sicl-boot-phase2)

(defparameter *bridge-classes* '())

(defun add-bridge-class (name function)
  (when (member name *bridge-classes* :key #'car :test #'equal)
    (error "Attempt to define bridge class ~s twice." name))
  (push (cons name function) *bridge-classes*))

(defun find-bridge-class (name)
  (let ((entry (assoc name *bridge-classes* :test #'equal)))
    (when (null entry)
      (error "There is no bridge class named ~s." name))
    (cdr entry)))

(defun delete-bridge-class (name)
  (setf *bridge-classes*
	(remove name *bridge-classes* :key #'car :test #'equal)))
