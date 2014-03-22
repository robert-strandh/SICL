(cl:in-package #:sicl-string)

;;; Extract an interval from a simple string to a fresh copy.  We
;;; assume that the caller has checked that the STRING is a simple
;;; string, and that START and END are valid bounding indices for
;;; STRING.
(defun extract-interval-simple (string start end)
  (assert (simple-string-p string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (let ((result (make-string (- end start))))
    (declare (type simple-string string)
	     ;; The CLHS says that the length of an array is a fixnum,
	     ;; so we know that START and END are both fixnums.
	     (type fixnum start end)
	     ;; Since we have checked that START and END are valid
	     ;; bounding indices for STRING, we can set SAFETY to 0,
	     ;; hoping that the compiler will not verify the indices
	     ;; of the accesses to the strings.
	     (optimize (speed 3) (safety 0) (debug 0)))
    ;; MAKE-STRING always returns a simple string.
    (declare (type simple-string result))
    (loop for source-index of-type fixnum from start below end
	  for destination-index of-type fixnum from 0
	  do (setf (schar result destination-index)
		   (schar string source-index)))
    result))

;;; Extract an interval from a general (i.e., not necessarily simple)
;;; string to a fresh copy.  We assume that the caller has checked
;;; that STRING is a string, and that START and END are valid bounding
;;; indices for STRING.
(defun extract-interval-general (string start end)
  (assert (stringp string))
  (assert (>= start 0))
  (assert (<= end (length string)))
  (assert (<= start end))
  (let ((result (make-string (- end start))))
    (declare (type string string)
	     ;; The CLHS says that the length of an array is a fixnum,
	     ;; so we know that START and END are both fixnums.
	     (type fixnum start end)
	     ;; Since we have checked that START and END are valid
	     ;; bounding indices for STRING, we can set SAFETY to 0,
	     ;; hoping that the compiler will not verify the indices
	     ;; of the accesses to the strings.
	     (optimize (speed 3) (safety 0) (debug 0)))
    ;; MAKE-STRING always returns a simple string.
    (declare (type simple-string result))
    (loop for source-index of-type fixnum from start below end
	  for destination-index of-type fixnum from 0
	  do (setf (schar result destination-index)
		   (char string source-index)))
    result))

(defun copy-string (string)
  (let ((result (make-string (length string))))
    (loop for i from 0 below (length string)
	  do (setf (char result i) (char string i)))
    result))

