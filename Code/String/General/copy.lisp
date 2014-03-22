(cl:in-package #:sicl-string)

;;; Extract an interval from a simple string to a fresh copy.  We
;;; assume that the caller has checked that START and END are valid
;;; bounding indices for the string.
(defun extract-interval-simple (string start end)
  (declare (type simple-string string)
	   (type fixnum start end)
	   (optimize (speed 3)))
  (let ((result (make-string (- end start))))
    ;; MAKE-STRING always returns a simple string.
    (declare (type simple-string result))
    (loop for source-index from start below end
	  for destination-index from 0
	  do (setf (schar result destination-index)
		   (schar string source-index)))
    result))

(defun copy-string (string)
  (let ((result (make-string (length string))))
    (loop for i from 0 below (length string)
	  do (setf (char result i) (char string i)))
    result))

