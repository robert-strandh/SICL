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

(defgeneric write-value (output environment value))

(defmethod write-value ((output cleavir-mir:lexical-location) environment value)
  (store-lexical output environment value))

(defgeneric execute-instruction (instruction environment))

(defun interpret-mir (enter-instruction environment arguments)
  (push (make-hash-table :test #'eq) environment)
  (let* ((lambda-list (cleavir-mir:lambda-list enter-instruction)) 
	 (pos (position-if (lambda (x) (member x '(&optional &key)))
			   lambda-list)))
    (cond ((null pos)
	   ;; Only required arguments.
	   (when (< (length arguments) (length lambda-list))
	     (error "too few arguments"))
	   (when (> (length arguments) (length lambda-list))
	     (error "too many arguments"))
	   (loop for value in arguments
		 for variable in lambda-list
		 do (store-lexical variable environment value)))
	  (t
	   (error "can't handle &optional or &key yet")))
    (let ((next (first (cleavir-mir:successors enter-instruction))))
      (catch 'return
	(loop do (execute-instruction next environment))))))

(defun enclose (enter-instruction environment)
  (lambda (&rest arguments)
    (interpret-mir enter-instruction environment arguments)))

(defmethod execute-instruction
    ((instruction cleavir-mir:assignment-instruction) environment)
  (let* ((input (car (cleavir-mir:inputs instruction)))
	 (value (read-value input environment))
	 (output (car (cleavir-mir:outputs instruction))))
    (write-value output environment value)
    (first (cleavir-mir:successors instruction))))

(defmethod execute-instruction
    ((instruction cleavir-mir:typeq-instruction) environment)
  (let* ((inputs (cleavir-mir:inputs instruction))
	 (datum (first inputs))
	 (type (second inputs))
	 (successors (cleavir-mir:successors instruction)))
    (if (typep (read-value datum environment) (read-value type environment))
	(first successors)
	(second successors))))
	       
    
