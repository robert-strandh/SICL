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

(defun read-with-source-tracking
    (&optional (a nil a-p) (b nil b-p) (c nil c-p) (d nil d-p))
  (let* ((*syntax-trees* (list nil))
	 (result (if a-p
		     (if b-p
			 (if c-p
			     (if d-p
				 (read a b c d)
				 (read a b c))
			     (read a b))
			 (read a))
		     (read))))
    (values result (car *syntax-trees*))))
