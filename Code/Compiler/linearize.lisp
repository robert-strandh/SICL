(defgeneric print-datum (datum stream))

(defmethod print-datum ((datum cleavir-ir:immediate-input) stream)
  (format stream "~a" (cleavir-ir:value datum)))

(defmethod print-datum ((datum cleavir-ir:global-input) stream)
  (format stream "[GLOBAL ~a]" (cleavir-ir:name datum)))

(defmethod print-datum ((datum cleavir-ir:constant-input) stream)
  (format stream "[CONSTANT ~a]" (cleavir-ir:value datum)))

(defmethod print-datum ((datum cleavir-ir:register-location) stream)
  (format stream "[REG ~a]" (cleavir-ir:name datum)))

(defmethod print-datum ((datum cleavir-ir:dynamic-location) stream)
  (format stream "[DYN ~a]" (cleavir-ir:index datum)))

(defvar *label-table*)

(defun assign-label (instruction)
  (unless (gethash instruction *label-table*)
    (setf (gethash instruction *label-table*) (gensym))))

(defvar *processed-p-table*)

(defgeneric linearize-instruction (instruction stream))

(defmethod linearize-instruction :around (instruction stream)
  (if (gethash instruction *processed-p-table*)
      (format stream "     GOTO ~a~%"
	      (gethash instruction *label-table*))
      (call-next-method)))

(defmethod linearize-instruction :before (instruction stream)
  (when (> (length (cleavir-ir:predecessors instruction)) 1)
    (assign-label instruction))
  (let ((label (gethash instruction *label-table*)))
    (unless (null label)
      (format stream "~a:~%" label))))

(defmethod linearize-instruction :after (instruction stream)
  (setf (gethash instruction *processed-p-table*) t)
  (let ((successors (cleavir-ir:successors instruction)))
    (unless (null successors)
      (unless (null (cdr successors))
	(assign-label (cadr successors))
	(format stream
		"            ALT: GOTO ~a~%"
		(gethash (cadr successors) *label-table*)))
      (linearize-instruction (car successors) stream))
      (unless (null (cdr successors))
	(unless (gethash (cadr successors) *processed-p-table*)
	  (linearize-instruction (cadr successors) stream)))))
	    
(defmethod linearize-instruction (instruction stream)
  (format stream "     ")
  (unless (null (cleavir-ir:outputs instruction))
    (loop for output in (cleavir-ir:outputs instruction)
	  do (print-datum output stream)
	     (format stream " "))
    (format stream "  <-  "))
  (format stream "~a  "
	  (class-name (class-of instruction)))
  (loop for input in (cleavir-ir:inputs instruction)
	do (print-datum input stream)
	   (format stream " "))
  (format stream "~%"))

(defmethod linearize-instruction
  ((instruction cleavir-ir:enter-instruction) stream)
  (format stream "     ENTER~%"))

(defmethod linearize-instruction
  ((instruction cleavir-ir:return-instruction) stream)
  (format stream "     RETURN~%"))

(defmethod linearize-instruction
  ((instruction cleavir-ir:funcall-instruction) stream)
  (format stream "     FUNCALL~%"))

(defmethod linearize-instruction
  ((instruction cleavir-ir:tailcall-instruction) stream)
  (format stream "     TAILCALL~%"))

(defun linearize (first-instruction)
  (let ((*label-table* (make-hash-table :test #'eq))
	(*processed-p-table* (make-hash-table :test #'eq)))
    (linearize-instruction first-instruction *standard-output*)))
      
