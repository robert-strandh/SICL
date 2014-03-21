(defgeneric print-datum (datum stream))

(defmethod print-datum ((datum sicl-mir:immediate-input) stream)
  (format stream "~a" (sicl-mir:value datum)))

(defmethod print-datum ((datum sicl-mir:global-input) stream)
  (format stream "[GLOBAL ~a]" (sicl-mir:name datum)))

(defmethod print-datum ((datum sicl-mir:constant-input) stream)
  (format stream "[CONSTANT ~a]" (sicl-mir:value datum)))

(defmethod print-datum ((datum sicl-mir:register-location) stream)
  (format stream "[REG ~a]" (sicl-mir:name datum)))

(defmethod print-datum ((datum sicl-mir:dynamic-location) stream)
  (format stream "[DYN ~a]" (sicl-mir:index datum)))

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
  (when (> (length (sicl-mir:predecessors instruction)) 1)
    (assign-label instruction))
  (let ((label (gethash instruction *label-table*)))
    (unless (null label)
      (format stream "~a:~%" label))))

(defmethod linearize-instruction :after (instruction stream)
  (setf (gethash instruction *processed-p-table*) t)
  (let ((successors (sicl-mir:successors instruction)))
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
  (unless (null (sicl-mir:outputs instruction))
    (loop for output in (sicl-mir:outputs instruction)
	  do (print-datum output stream)
	     (format stream " "))
    (format stream "  <-  "))
  (format stream "~a  "
	  (class-name (class-of instruction)))
  (loop for input in (sicl-mir:inputs instruction)
	do (print-datum input stream)
	   (format stream " "))
  (format stream "~%"))

(defmethod linearize-instruction
  ((instruction sicl-mir:enter-instruction) stream)
  (format stream "     ENTER~%"))

(defmethod linearize-instruction
  ((instruction sicl-mir:return-instruction) stream)
  (format stream "     RETURN~%"))

(defmethod linearize-instruction
  ((instruction sicl-mir:funcall-instruction) stream)
  (format stream "     FUNCALL~%"))

(defmethod linearize-instruction
  ((instruction sicl-mir:tailcall-instruction) stream)
  (format stream "     TAILCALL~%"))

(defun linearize (first-instruction)
  (let ((*label-table* (make-hash-table :test #'eq))
	(*processed-p-table* (make-hash-table :test #'eq)))
    (linearize-instruction first-instruction *standard-output*)))
      
