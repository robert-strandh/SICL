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

(defmethod execute-simple-instruction
    ((instruction cleavir-ir:cdr-instruction)
     inputs outputs
     process)
  (setf (lexical-value (first outputs) process)
	(cdr (lexical-value (first inputs) process))))

(defun parse-arguments (arguments lambda-list process)
  (let ((remaining-arguments arguments)
	(remaining-lambda-list lambda-list))
    (tagbody
     required
       (cond ((and (null remaining-arguments) (null remaining-lambda-list))
	      (go out))
	     ((null remaining-lambda-list)
	      (error "too many arguments"))
	     ((eq (first remaining-lambda-list) '&optional)
	      (pop remaining-lambda-list)
	      (go optional))
	     ((eq (first remaining-lambda-list) '&rest)
	      (pop remaining-lambda-list)
	      (go rest))
	     ((eq (first remaining-lambda-list) '&key)
	      (pop remaining-lambda-list)
	      (go key))
	     (t
	      (setf (lexical-value (first remaining-lambda-list) process)
		    (first remaining-arguments))
	      (pop remaining-lambda-list)
	      (pop remaining-arguments)
	      (go required)))
     optional
       (cond ((and (null remaining-arguments) (null remaining-lambda-list))
	      (go out))
	     ((null remaining-lambda-list)
	      (error "too many arguments"))
	     ((eq (first remaining-lambda-list) '&rest)
	      (pop remaining-lambda-list)
	      (go rest))
	     ((eq (first remaining-lambda-list) '&key)
	      (pop remaining-lambda-list)
	      (go key))
	     ((null remaining-arguments)
	      (setf (lexical-value (second (first remaining-lambda-list)) process)
		    nil)
	      (pop remaining-lambda-list)
	      (go optional))
	     (t
	      (setf (lexical-value (first (first remaining-lambda-list)) process)
		    (first remaining-arguments))
	      (setf (lexical-value (second (first remaining-lambda-list)) process)
		    t)
	      (pop remaining-lambda-list)
	      (pop remaining-arguments)
	      (go optional)))
     rest
       (setf (lexical-value (first remaining-lambda-list) process)
	     (copy-list remaining-arguments))
       (pop remaining-lambda-list)
       (cond ((null remaining-lambda-list)
	      (go out))
	     (t
	      (pop remaining-lambda-list)
	      (go key)))
     key
       (cond ((and (null remaining-arguments) (null remaining-lambda-list))
	      (go out))
	     ((null remaining-arguments)
	      (setf (lexical-value (third (first remaining-lambda-list)) process)
		    nil)
	      (pop remaining-lambda-list)
	      (go key))
	     ((null remaining-lambda-list)
	      (if (getf remaining-lambda-list :allow-other-keys)
		  (go out)
		  (error "unknown keyword argument ~s"
			 (first remaining-arguments))))
	     ((eq (first remaining-lambda-list) '&allow-other-keys)
	      (go out))
	     (t (destructuring-bind (keyword variable supplied-p)
		    (first lambda-list)
		  (let* ((default (list nil))
			 (value (getf remaining-arguments keyword default)))
		    (if (eq value default)
			(setf (lexical-value supplied-p process)
			      nil)
			(setf (lexical-value variable process)
			      value
			      (lexical-value supplied-p process)
			      t))
		    (loop while (remf keyword remaining-arguments))
		    (pop remaining-lambda-list)
		    (go key)))))
     out)))

(defmethod execute-simple-instruction 
    ((instruction enter-instruction)
     inputs outputs
     process)
  ;; Fill the top level of the static environment with
  ;; the owned variables of the enter instruction.
  (loop with static-env = (static-env (stack process))
	for variable in (owned-variables instruction)
	do (setf (gethash variable (car static-env)) nil))
  ;; Parse the argument list
  (parse-arguments (arguments (stack process))
		   (cleavir-ir:lambda-list instruction)
		   process))

(defgeneric execute-call-instruction
    (instruction function inputs outputs process))

(defmethod execute-call-instruction
    (instruction (function built-in-function) inputs outputs process)
  (let* ((fun (host-function function))
	 (args (loop for input in inputs
		     collect (lexical-value input process)))
	 (values (multiple-value-list (apply fun args))))
    ;; Set all outputs to nil in case there are fewer values than
    ;; outputs.
    (loop for output in outputs
	  do (setf (lexical-value output process) nil))
    (loop for value in values
	  for output in outputs
	  do (setf (lexical-value output process) value))))

(defmethod execute-call-instruction
    (instruction (function interpreted-function) inputs outputs process)
  (setf (return-variables (stack process)) outputs)
  (let* ((dynamic-env (dynamic-env (car (stack process))))
	 (static-env (cons (make-hash-table :test #'eq)
			   (environment function)))
	 (args (loop for input in inputs
		     collect (lexical-value input process)))
	 (new-frame (make-instance 'stack-frame
		      :static-env static-env
		      :dynamic-env dynamic-env
		      :next-instruction (entry-point function)
		      :arguments args)))
    (push new-frame (stack process))))

(defmethod execute-simple-instruction
    ((instruction cleavir-ir:funcall-instruction)
     inputs outputs
     process)
  (let ((function (lexical-value (first inputs) process)))
    (execute-call-instruction
     instruction function (rest inputs) outputs process)))
