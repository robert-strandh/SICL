(cl:in-package #:cleavir-mir-interpreter)

(defun load-lexical (lexical environment)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p
	       (return value)))
	finally (error "Attempt to use an undefined variable")))

(defun store-lexical (lexical environment value)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p
	       (setf (gethash lexical level) value)
	       (return (values))))
	finally (setf (gethash lexical (car environment)) value)
		(return (values))))

(defgeneric execute-instruction (instruction environment))
