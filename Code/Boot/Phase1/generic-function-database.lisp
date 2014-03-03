(cl:in-package #:sicl-boot-phase1)

(defparameter *bridge-generic-functions* '())

(defun add-bridge-generic-function (name function)
  (pushnew (cons name function) *bridge-generic-functions*
	   :key #'car :test #'equal)
  (setf (fdefinition name) function))

(defun find-bridge-generic-function (name &optional (error-p t))
  (let ((entry (assoc name *bridge-generic-functions* :test #'equal)))
    (if (null entry)
	(if error-p
	    (error "There is no bridge generic function named ~s." name)
	    nil)
	(cdr entry))))

(defun delete-bridge-generic-function (name)
  (setf *bridge-generic-functions*
	(remove name *bridge-generic-functions* :key #'car :test #'equal)))
