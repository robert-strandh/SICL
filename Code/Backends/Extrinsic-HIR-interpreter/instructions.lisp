(cl:in-package #:sicl-extrinsic-hir-interpreter)

(defclass enter-instruction (cleavir-ir:enter-instruction)
  ((%owned-variables :initarg :owned-variables :reader owned-variables)))

(defun lexical-value (lexical-variable process)
  (loop for table in (static-env (stack process))
	do (multiple-value-bind (value present-p)
	       (gethash lexical-variable table)
	     (when present-p
	       (return-from lexical-value value))))
  (error "unknown variable ~s" lexical-variable))
