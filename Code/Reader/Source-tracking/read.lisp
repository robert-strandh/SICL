(cl:in-package #:sicl-reader)

(defparameter *syntax-trees* '())

(defun read (&optional
	       (input-stream *standard-input*)
	       (eof-error-p t)
	       (eof-value nil)
	       (recursive-p nil))
  (let ((*preserve-whitespace* recursive-p)
	(*syntax-trees* (cons (list nil) *syntax-trees*)))
    (let ((result (read-common input-stream eof-error-p eof-value)))
      (push result (cadr *syntax-trees*))
      result)))
