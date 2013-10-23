(cl:in-package #:sicl-x86-64)

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

(defgeneric generate-instruction (instruction stream))

(defmethod generate-instruction :around (instruction stream)
  (if (gethash instruction *processed-p-table*)
      (format stream "     JMP ~a~%"
	      (gethash instruction *label-table*))
      (call-next-method)))

(defmethod generate-instruction :before (instruction stream)
  (when (> (length (sicl-mir:predecessors instruction)) 1)
    (assign-label instruction))
  (let ((label (gethash instruction *label-table*)))
    (unless (null label)
      (format stream "~a:~%" label)))
  (setf (gethash instruction *processed-p-table*) t))

(defmethod generate-instruction :after (instruction stream)
  (let ((successors (sicl-mir:successors instruction)))
    (unless (null successors)
      (generate-instruction (car successors) stream))
      (unless (null (cdr successors))
	(unless (gethash (cadr successors) *processed-p-table*)
	  (generate-instruction (cadr successors) stream)))))
	    
(defmethod generate-instruction
    ((instruction sicl-mir:s<-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (format stream "     CMP ~d, ~a~%"
		  (sicl-mir:value input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~d, [RBP ~d]~%"
		  (sicl-mir:value input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP ~a, ~d~%"
		  (sicl-mir:name input1)
		  (sicl-mir:value input2)))
	 (sicl-mir:register-location
	  (format stream "     CMP ~a, ~a~%"
		  (sicl-mir:name input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~a, [RBP ~d]~%"
		  (sicl-mir:name input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:value input1)))
	 (sicl-mir:register-location
	  (format stream "     CMP [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:name input1)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input1))))
	  (format stream "     CMP RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input2))))))))
    (assign-label (cadr successors))
    (format stream "     JNL ~a~%"
	    (gethash (cadr successors) *label-table*))))

(defmethod generate-instruction
    ((instruction sicl-mir:s<=-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (format stream "     CMP ~d, ~a~%"
		  (sicl-mir:value input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~d, [RBP ~d]~%"
		  (sicl-mir:value input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP ~a, ~d~%"
		  (sicl-mir:name input1)
		  (sicl-mir:value input2)))
	 (sicl-mir:register-location
	  (format stream "     CMP ~a, ~a~%"
		  (sicl-mir:name input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~a, [RBP ~d]~%"
		  (sicl-mir:name input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:value input1)))
	 (sicl-mir:register-location
	  (format stream "     CMP [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:name input1)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input1))))
	  (format stream "     CMP RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input2))))))))
    (assign-label (cadr successors))
    (format stream "     JNLE ~a~%"
	    (gethash (cadr successors) *label-table*))))

(defmethod generate-instruction
    ((instruction sicl-mir:u<-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (format stream "     CMP ~d, ~a~%"
		  (sicl-mir:value input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~d, [RBP ~d]~%"
		  (sicl-mir:value input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP ~a, ~d~%"
		  (sicl-mir:name input1)
		  (sicl-mir:value input2)))
	 (sicl-mir:register-location
	  (format stream "     CMP ~a, ~a~%"
		  (sicl-mir:name input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~a, [RBP ~d]~%"
		  (sicl-mir:name input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:value input1)))
	 (sicl-mir:register-location
	  (format stream "     CMP [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:name input1)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input1))))
	  (format stream "     CMP RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input2))))))))
    (assign-label (cadr successors))
    (format stream "     JNB ~a~%"
	    (gethash (cadr successors) *label-table*))))

(defmethod generate-instruction
    ((instruction sicl-mir:u<=-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (format stream "     CMP ~d, ~a~%"
		  (sicl-mir:value input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~d, [RBP ~d]~%"
		  (sicl-mir:value input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP ~a, ~d~%"
		  (sicl-mir:name input1)
		  (sicl-mir:value input2)))
	 (sicl-mir:register-location
	  (format stream "     CMP ~a, ~a~%"
		  (sicl-mir:name input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~a, [RBP ~d]~%"
		  (sicl-mir:name input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:value input1)))
	 (sicl-mir:register-location
	  (format stream "     CMP [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:name input1)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input1))))
	  (format stream "     CMP RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input2))))))))
    (assign-label (cadr successors))
    (format stream "     JNBE ~a~%"
	    (gethash (cadr successors) *label-table*))))

(defmethod generate-instruction
    ((instruction sicl-mir:==-instruction) stream)
  (let* ((inputs (sicl-mir:inputs instruction))
	 (input1 (first inputs))
	 (input2 (second inputs))
	 (successors (sicl-mir:successors instruction)))
    (etypecase input1
      (sicl-mir:immediate-input
       (etypecase input2
	 (sicl-mir:register-location
	  (format stream "     CMP ~d, ~a~%"
		  (sicl-mir:value input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~d, [RBP ~d]~%"
		  (sicl-mir:value input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:register-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP ~a, ~d~%"
		  (sicl-mir:name input1)
		  (sicl-mir:value input2)))
	 (sicl-mir:register-location
	  (format stream "     CMP ~a, ~a~%"
		  (sicl-mir:name input1)
		  (sicl-mir:name input2)))
	 (sicl-mir:dynamic-location
	  (format stream "     CMP ~a, [RBP ~d]~%"
		  (sicl-mir:name input1)
		  (* -8 (1+ (sicl-mir:index input2)))))))
      (sicl-mir:dynamic-location
       (etypecase input2
	 (sicl-mir:immediate-input
	  (format stream "     CMP [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:value input1)))
	 (sicl-mir:register-location
	  (format stream "     CMP [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index input2)))
		  (sicl-mir:name input1)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input1))))
	  (format stream "     CMP RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input2))))))))
    (assign-label (cadr successors))
    (format stream "     JNE ~a~%"
	    (gethash (cadr successors) *label-table*))))

(defmethod generate-instruction (instruction stream)
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

(defmethod generate-instruction
    ((instruction sicl-mir:enter-instruction) stream)
  ;; For now, don't generate any code.  Later, this is where we
  ;; allocate stack space for dynamic locations. 
  (declare (ignore stream))
  nil)

(defmethod generate-instruction
    ((instruction sicl-mir:return-instruction) stream)
  (format stream "     RET~%"))

(defmethod generate-instruction
    ((instruction sicl-mir:funcall-instruction) stream)
  ;; When we come here, the function object is already contained in
  ;; *lv-fun-reg*.  The end result we want is that the linkage vector
  ;; be in *lv-fun-reg* and that we issue a CALL instruction to the
  ;; entry point of the function.
  (let ((name (sicl-mir:name *lv-fun-reg*)))
    ;; Start by loading the contents vector of the function object to
    ;; *lv-fun-reg*.
    (format stream "     MOV ~a, [~a 1]~%" name name)
    ;; Next, load the entry point address to RAX.
    ;; FIXME: check the offset of the entry point.
    ;; FIXME: generate the offset of the entry point automatically.
    (format stream "     MOV RAX, [~a 40]~%" name)
    ;; Next, load the static environment into *senv-reg*.
    ;; FIXME: check the offset of the static environment.
    ;; FIXME: generate the offset of the static environment automatically.
    (format stream "     MOV ~a, [~a 24]~%" (sicl-mir:name *senv-reg*) name)
    ;; Next, load the linkage vector into *lv-fun-reg*.
    ;; FIXME: check the offset of the linkage vector.
    ;; FIXME: generate the offset of the linkage vector automatically.
    (format stream "     MOV ~a, [~a 32]~%" name name)
    ;; Finally, call the entry point.
    (format stream "     CALL RAX~%")))

(defmethod generate-instruction
    ((instruction sicl-mir:assignment-instruction) stream)
  (let ((input (first (sicl-mir:inputs instruction)))
	(output (first (sicl-mir:outputs instruction))))
    (etypecase input
      (sicl-mir:immediate-input
       (etypecase output
	 (sicl-mir:register-location
	  (format stream "     MOV ~a, ~d~%"
		  (sicl-mir:name output)
		  (sicl-mir:value input)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV [RBP ~d], ~d~%"
		  (* -8 (1+ (sicl-mir:index output)))
		  (sicl-mir:value input)))))
      (sicl-mir:register-location
       (etypecase output
	 (sicl-mir:register-location
	  (format stream "     MOV ~a, ~a~%"
		  (sicl-mir:name output)
		  (sicl-mir:name input)))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV [RBP ~d], ~a~%"
		  (* -8 (1+ (sicl-mir:index output)))
		  (sicl-mir:name input)))))
      (sicl-mir:dynamic-location
       (etypecase output
	 (sicl-mir:register-location
	  (format stream "     MOV ~a, [RBP ~d]~%"
		  (sicl-mir:name output)
		  (* -8 (1+ (sicl-mir:index input)))))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input))))
	  (format stream "     MOV [RBP ~d], RAX~%"
		  (* -8 (1+ (sicl-mir:index output))))))))))

(defmethod generate-instruction
    ((instruction sicl-mir:memref-instruction) stream)
  (let ((input (first (sicl-mir:inputs instruction)))
	(displacement (sicl-mir:displacement instruction))
	(output (first (sicl-mir:outputs instruction))))
    (etypecase input
      (sicl-mir:register-location
       (etypecase output
	 (sicl-mir:register-location
	  (format stream "     MOV ~a, [~a ~d]~%"
		  (sicl-mir:name output)
		  (sicl-mir:name input)
		  displacement))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [~a ~d]~%"
		  (sicl-mir:name input)
		  displacement)
	  (format stream "     MOV [RBP ~d], RAX~%"
		  (* -8 (1+ (sicl-mir:index output)))))))
      (sicl-mir:dynamic-location
       (etypecase output
	 (sicl-mir:register-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input))))
	  (format stream "     MOV ~a, [RAX ~d]~%"
		  (sicl-mir:name output)
		  displacement))
	 (sicl-mir:dynamic-location
	  (format stream "     MOV RAX, [RBP ~d]~%"
		  (* -8 (1+ (sicl-mir:index input))))
	  (format stream "     MOV RAX, [RAX ~d]~%"
		  displacement)
	  (format stream "     MOV [RBP ~d], RAX~%"
		  (* -8 (1+ (sicl-mir:index output))))))))))


(defmethod generate-instruction
    ((instruction sicl-mir:tailcall-instruction) stream)
  (format stream "     TAILCALL~%"))

(defun generate-code (first-instruction)
  (let ((*label-table* (make-hash-table :test #'eq))
	(*processed-p-table* (make-hash-table :test #'eq)))
    (generate-instruction first-instruction *standard-output*)))
      
