(cl:in-package #:sicl-environment)

(defun find-package (client environment name)
  (declare (ignore client))
  (gethash name (clostrum-basic::packages environment)))

(defun (setf find-package) (package client environment name)
  (declare (ignore client))
  (setf (gethash name (clostrum-basic::packages environment))
        package))
