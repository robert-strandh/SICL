(cl:in-package #:sicl-string)

(defun copy-string (string)
  (let ((result (make-string (length string))))
    (loop for i from 0 below (length string)
	  do (setf (char result i) (char string i)))
    result))

