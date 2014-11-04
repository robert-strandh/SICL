(cl:in-package #:sicl-direct-extrinsic-compiler)

(defun process-fdefinition-instruction (instruction static-env constants)
  (let* ((inputs (cleavir-ir:inputs instruction))
	 (function-name (cleavir-ir:value (first inputs)))
	 (output (first (cleavir-ir:outputs instruction)))
	 (temp1 (cleavir-ir:new-temporary))
	 (temp2 (cleavir-ir:new-temporary)))
    ;; Insert the instruction that takes the CAR of the
    ;; function cell. 
    (cleavir-ir:insert-instruction-before
     (cleavir-ir:make-car-instruction
      temp1 output instruction)
     instruction)
    ;; Insert the instruction that accesses the element in
    ;; the static runtime environment.
    (cleavir-ir:insert-instruction-before
     (cleavir-ir:make-t-aref-instruction
      temp2
      (make-instance 'cleavir-ir:immediate-input
	:value (ash (1+ (length constants)) 1))
      temp1 instruction)
     instruction)
    ;; Insert the instruction that takes the CAR of the
    ;; static runtime environment to access the first vector
    ;; FIXME: it is not always the first vector. 
    (cleavir-ir:insert-instruction-before
     (cleavir-ir:make-car-instruction
      static-env temp2 instruction)
     instruction)
    ;; Return the modified constants list
    (cons `(function ,function-name) constants)))
  
(defun process-input (instruction input static-env constants)
  (if (typep input 'cleavir-ir:constant-input)
      (let ((temp (cleavir-ir:new-temporary)))
	;; Insert the instruction that accesses the element in
	;; the static runtime environment.
	(cleavir-ir:insert-instruction-before
	 (cleavir-ir:make-t-aref-instruction
	  temp
	  (make-instance 'cleavir-ir:immediate-input
	    :value (ash (1+ (length constants)) 1))
	  (first (cleavir-ir:outputs instruction))
	  instruction)
	 instruction)
	;; Insert the instruction that takes the CAR of the
	;; static runtime environment to access the first vector
	;; FIXME: it is not always the first vector. 
	(cleavir-ir:insert-instruction-before
	 (cleavir-ir:make-car-instruction
	  static-env temp instruction)
	 instruction)
	(values temp (cons `(constant ,(cleavir-ir:value input)) constants)))
      (values input constants)))

(defun process-constant-inputs (instruction static-env constants)
  (setf (cleavir-ir:inputs instruction)
	(loop for input in (cleavir-ir:inputs instruction)
	      collect (multiple-value-bind (new-input new-constants)
			  (process-input instruction input static-env constants)
			(setf constants new-constants)
			new-input))))

(defun process-instruction (instruction static-env constants)
    (cond ((and (typep instruction 'cleavir-ir:fdefinition-instruction)
		(typep (first (cleavir-ir:inputs instruction))
		       'cleavir-ir:constant-input))
	   (process-fdefinition-instruction instruction static-env constants))
	  (t
	   (process-constant-inputs instruction static-env constants))))
	     
(defun process-constants (initial-instruction)
  (let ((static-env (cleavir-ir:new-temporary))
	(table (make-hash-table :test #'eq))
	(constants '()))
    (push static-env (cleavir-ir:outputs initial-instruction))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (setf constants
		       (process-instruction
			instruction static-env constants))
		 (mapc #'traverse (cleavir-ir:successors instruction)))))
      (traverse initial-instruction))))

