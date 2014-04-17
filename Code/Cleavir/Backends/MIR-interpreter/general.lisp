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

(defgeneric read-value (input environment))

(defmethod read-value ((input cleavir-mir:lexical-location) environment)
  (load-lexical input environment))

(defmethod read-value ((input cleavir-mir:constant-input) environment)
  (declare (ignore environment))
  (cleavir-mir:value input))

(defgeneric execute-instruction (instruction environment))
