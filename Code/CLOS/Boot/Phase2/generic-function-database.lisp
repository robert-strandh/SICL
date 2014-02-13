(cl:in-package #:sicl-clos)

(defparameter *bridge-generic-functions* '())

(defun add-bridge-generic-function (name function)
  (pushnew (cons name function) *bridge-generic-functions*
	   :key #'car :test #'equal))

(defun find-bridge-generic-function (name)
  (let ((entry (assoc name *bridge-generic-functions* :test #'equal)))
    (when (null entry)
      (error "There is no bridge generic function named ~s." name))
    (cdr entry)))

(defun delete-bridge-generic-function (name)
  (setf *bridge-generic-functions*
	(remove name *bridge-generic-functions* :key #'car :test #'equal)))
