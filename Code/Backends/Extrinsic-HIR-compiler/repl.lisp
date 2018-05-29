(cl:in-package #:sicl-extrinsic-hir-compiler)

(defun repl ()
  (loop do (format *query-io* "SICL: ")
	   (finish-output *query-io*)
	   (let ((form (eclector.reader:read *query-io*)))
	     (if (eq form 'quit)
		 (return-from repl)
		 (let ((values (multiple-value-list (eval form))))
		   (loop for value in values
			 do (format *query-io* "~s~%" value)))))))
