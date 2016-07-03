(cl:in-package #:sicl-source-tracking-reader)

(defparameter *syntax-trees* '())

(defun sicl-reader:read
    (&optional
       (input-stream *standard-input*)
       (eof-error-p t)
       (eof-value nil)
       (recursive-p nil))
  (let ((sicl-reader::*preserve-whitespace* recursive-p)
	(*syntax-trees* (cons (list nil) *syntax-trees*)))
    (let ((result (sicl-reader::read-common input-stream eof-error-p eof-value)))
      (push result (cadr *syntax-trees*))
      result)))

(defun read-with-source-tracking
    (&optional
       (stream *standard-input* stream-p)
       (b nil b-p) (c nil c-p) (d nil d-p))
  (let* ((*syntax-trees* (list nil))
	 (result (if stream-p
		     (if b-p
			 (if c-p
			     (if d-p
				 (read stream b c d)
				 (read stream b c))
			     (read stream b))
			 (read stream))
		     (read))))
    (values result (car *syntax-trees*))))
