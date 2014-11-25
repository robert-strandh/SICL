(cl:in-package #:cleavir-generate-ast)

(defun ast-from-stream (stream environment)
  (cleavir-ast:make-progn-ast
   (let ((eof (list nil)))
     (loop for form = (read stream nil eof)
	   until (eq form eof)
	   collect (generate-ast form environment)))))


(defun ast-from-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (ast-from-stream stream environment)))
