(cl:in-package #:cleavir-generate-ast)

(defun ast-from-stream (stream environment system)
  (let ((*compiler* 'compile-file))
    (process-progn
     (let ((eof (list nil)))
       (loop for form = (read stream nil eof)
	     until (eq form eof)
	     collect (generate-ast form environment system))))))

(defun ast-from-file (filename environment system)
  (with-open-file (stream filename :direction :input)
    (ast-from-stream stream environment system)))
