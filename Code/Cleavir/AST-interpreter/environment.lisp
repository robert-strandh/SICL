(cl:in-package #:cleavir-ast-interpreter)

(defun lookup-lexical (lexical environment)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p (return value)))
	finally (error "Unknown lexical variable ~s" lexical)))

(defun set-lexical (lexical value environment)
  (loop for level in environment
	do (multiple-value-bind (value present-p)
	       (gethash lexical level)
	     (when present-p
	       (setf (gethash lexical level) value)
	       (return)))
	finally (setf (gethash lexical (first environment) ) value)))
