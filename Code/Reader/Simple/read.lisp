(cl:in-package #:sicl-reader)

(defvar *labels*)

(defun read (&optional
	       (input-stream *standard-input*)
	       (eof-error-p t)
	       (eof-value nil)
	       (recursive-p nil))
  (if recursive-p
      (let ((*preserve-whitespace* recursive-p))
        (read-common input-stream eof-error-p eof-value))
      (let ((*preserve-whitespace* recursive-p)
            (*labels* (make-hash-table)))
        (read-common input-stream eof-error-p eof-value))))

(defun read-preserving-whitespace (&optional
                                     (input-stream *standard-input*)
                                     (eof-error-p t)
                                     (eof-value nil)
                                     (recursive-p nil))
  (if recursive-p
      (let ((*preserve-whitespace* t))
        (read-common input-stream eof-error-p eof-value))
      (let ((*preserve-whitespace* t)
            (*labels* (make-hash-table)))
        (read-common input-stream eof-error-p eof-value))))
