(cl:in-package #:sicl-clos)

(defparameter *target-generic-functions* '())

(defun add-target-generic-function (name function)
  (pushnew (cons name function) *target-generic-functions*
	   :key #'car :test #'equal))

(defun find-target-generic-function (name)
  (let ((entry (assoc name *target-generic-functions* :test #'equal)))
    (when (null entry)
      (error "There is no target generic function named ~s." name))
    (cdr entry)))

(defun delete-target-generic-function (name)
  (setf *target-generic-functions*
	(remove name *target-generic-functions* :key #'car :test #'equal)))
