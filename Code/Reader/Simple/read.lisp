(cl:in-package #:sicl-reader)

(defun read (&optional
	       (input-stream *standard-input*)
	       (eof-error-p t)
	       (eof-value nil)
	       (recursive-p nil))
  (let ((*preserve-whitespace* recursive-p))
    (read-common input-stream eof-error-p eof-value)))

(defun read-preserving-whitespace (&optional
                                     (input-stream *standard-input*)
                                     (eof-error-p t)
                                     (eof-value nil)
                                     (recursive-p nil))
  (declare (ignore recursive-p))
  (let ((*preserve-whitespace* t))
    (read-common input-stream eof-error-p eof-value)))
