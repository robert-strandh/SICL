(cl:in-package #:sicl-string)

;;; Extract an interval from a simple string to a fresh copy.
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

(defun copy-string (string)
  (extract-interval-simple string 0 (length string)))
