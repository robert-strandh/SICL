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
  
(defmethod execute-simple-instruction
    ((instruction cleavir-ir:enclose-instruction)
     inputs outputs
     process)
  (setf (lexical-value (first outputs) process)
	(make-instance 'interpreted-function
	  :entry-point (cleavir-ir:code instruction)
	  :environment (static-env (stack process)))))

(defmethod execute-simple-instruction
    ((instruction cleavir-ir:assignment-instruction)
     inputs outputs
     process)
  (setf (lexical-value (first outputs) process)
	(lexical-value (first inputs) process)))

(defmethod execute-simple-instruction
    ((instruction cleavir-ir:return-instruction)
     inputs outputs
     process)
  (let ((return-values (loop for output in outputs
			     collect (lexical-value output process))))
    (pop (stack process))
    ;; Set all variables to nil in case there are fewer values than
    ;; outputs.
    (loop for return-var in (return-variables (stack process))
	  do (setf (lexical-value return-var process) nil))
    (loop for value in return-values
	  for return-var in (return-variables (stack process))
	  do (setf (lexical-value return-var process) value))))

(defmethod execute-simple-instruction
    ((instruction cleavir-ir:car-instruction)
     inputs outputs
     process)
  (setf (lexical-value (first outputs) process)
	(car (lexical-value (first inputs) process))))
