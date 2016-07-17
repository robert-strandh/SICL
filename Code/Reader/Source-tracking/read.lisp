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
       (input-stream *standard-input*)
       (eof-error-p t)
       (eof-value nil)
       (recursive-p nil))
  (let ((sicl-reader::*preserve-whitespace* recursive-p)
	(*syntax-trees* (cons '() *syntax-trees*)))
    (let ((result (sicl-reader::read-common input-stream eof-error-p eof-value)))
      (push (if (and (consp result)
		     (total-correspondance result (reverse (car *syntax-trees*))))
		(make-instance 'cleavir-cst:cst
		  :expression result
		  :location nil
		  :children (reverse (car *syntax-trees*)))
		(make-instance 'cleavir-cst:cst
		  :expression result
		  :location nil
		  :children '()))
	    (second *syntax-trees*))
      result)))

(defun read-with-source-tracking
    (&optional
       (stream *standard-input*)
       (b nil b-p) (c nil c-p) (d nil d-p))
  (let* ((*syntax-trees* (list nil))
	 (result (if b-p
		     (if c-p
			 (if d-p
			     (sicl-reader:read stream b c d)
			     (sicl-reader:read stream b c))
			 (sicl-reader:read stream b))
		     (sicl-reader:read stream))))
    (values result (car *syntax-trees*))))
