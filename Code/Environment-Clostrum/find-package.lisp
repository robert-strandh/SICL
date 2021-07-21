(cl:in-package #:sicl-environment)

(defun find-package (client environment name)
  (declare (ignore client))
  (gethash name (clostrum/virtual::packages environment)))

(defun (setf find-package) (package client environment name)
  (declare (ignore client))
  (setf (gethash name (clostrum/virtual::packages environment))
        package))
