(cl:in-package #:cleavir-skim-file)

(defgeneric skim-form (form environment))

(defun skim-file (filename environment)
  (with-open-file (stream filename :direction :input)
    (loop with eof-value = (list)
	  for top-level-form = (read stream nil eof-value)
	  until (eq top-level-form eof-value)
	  do (skim-form top-level-form environment))))
