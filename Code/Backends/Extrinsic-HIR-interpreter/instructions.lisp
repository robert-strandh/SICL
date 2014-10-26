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

(defun (setf lexical-value) (value lexical-variable process)
  (loop for table in (static-env (stack process))
	do (multiple-value-bind (old-value present-p)
	       (gethash lexical-variable table)
	     (declare (ignore old-value))
	     (when present-p
	       (setf (gethash lexical-variable table) value)
	       (return-from lexical-value)))))

(defgeneric execute-simple-instruction (instruction inputs outputs process))

(defmethod execute-simple-instruction :before
    (instruction inputs outputs process)
  (declare (ignore inputs outputs))
  (setf (next-instruction (stack process))
	(first (cleavir-ir:successors instruction))))
