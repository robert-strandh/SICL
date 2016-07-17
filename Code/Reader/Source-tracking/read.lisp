(cl:in-package #:sicl-source-tracking-reader)

(defun total-correspondance (forms syntax-trees)
  (and (null (cdr (last forms)))
       (= (length forms) (length syntax-trees))
       (every (lambda (form syntax-tree)
		(eql form (cleavir-cst:expression syntax-tree)))
	      forms
	      syntax-trees)))

(defparameter *syntax-trees* '())

(defun sicl-reader:read
    (&optional
       (stream *standard-input*)
       (eof-error-p t)
       (eof-value nil)
       (recursive-p nil))
  (let ((sicl-reader::*preserve-whitespace* recursive-p)
	(*syntax-trees* (cons '() *syntax-trees*)))
    (let* ((start-line (line stream))
	   (start-column (column stream))
	   (result (sicl-reader::read-common stream eof-error-p eof-value))
	   (end-line (line stream))
	   (end-column (column stream))
	   (location (make-instance 'location
		       :start-line start-line
		       :start-column start-column
		       :end-line end-line
		       :end-column end-column)))
      (push (if (and (consp result)
		     (total-correspondance result (reverse (car *syntax-trees*))))
		(make-instance 'cleavir-cst:cst
		  :expression result
		  :location location
		  :children (reverse (car *syntax-trees*)))
		(make-instance 'cleavir-cst:cst
		  :expression result
		  :location location
		  :children '()))
	    (second *syntax-trees*))
      result)))

(defun read-with-source-tracking
    (&optional
       (stream *standard-input*)
       (b nil b-p) (c nil c-p) (d nil d-p))
  (let* ((*syntax-trees* (list nil))
	 (new-stream (make-source-tracking-stream stream))
	 (result (if b-p
		     (if c-p
			 (if d-p
			     (sicl-reader:read new-stream b c d)
			     (sicl-reader:read new-stream b c))
			 (sicl-reader:read new-stream b))
		     (sicl-reader:read new-stream))))
    (values result (car *syntax-trees*))))
