(in-package #:sicl-arm-lir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The ARM backend.

(defvar *registers*
  (vector (sicl-mir:make-register-location 'sicl-arm-assembler:r0)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r1)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r2)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r3)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r4)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r5)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r6)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r7)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r8)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r9)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r10)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r11)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:r12)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:SP)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:LR)
	  (sicl-mir:make-register-location 'sicl-arm-assembler:PC)))

(defclass backend-arm ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Possibly convert MIR constants to immediates.
;;;
;;; FIXME: This code is wrong.  The ARM instructions that take
;;; immediate input are quite complicated, and it is not trivial to
;;; express what constants are acceptable as immediate inputs. 

(defmethod sicl-program:convert-constant ((backend backend-arm) constant)
  (if (and (or (typep constant 'sicl-mir:constant-input)
	       (typep constant 'sicl-mir:word-input))
	   (typep (sicl-mir:value constant) 'integer)
	   (<= (- (expt 2 11)) (sicl-mir:value constant) (- (expt 2 11) 1)))
      (sicl-mir:make-immediate-input (sicl-mir:value constant))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert a MIR instruction graph to LIR.
;;;
;;; When we want to alter the instruction graph, we can't really
;;; insert instructions BEFORE another instruction.  The reason is
;;; that, although the predecessors are known initially, once we have
;;; altered the instruction graph, this is no longer the case.  For
;;; that reason, we are obliged to always insert instructions AFTER
;;; other instructions.  This leads to strange code where the
;;; instruction to be alterd is first cloned and then it has its class
;;; changed.
;;;
;;; However, things are not as bad as they seem.  The MIR package
;;; provides a function CLONE-INSTRUCTION that we can use. 
;;;
;;; So if we have an instruction A and we would have wanted to precede
;;; A with some other instruction B, we do as follows:
;;;
;;;   * Clone A, giving us A'.
;;;
;;;   * Change the class of A to that of B, passing appropriate
;;;     initargs, including A' as its successor. 

(defun add-instruction-before-instruction
    (instruction new-class inputs outputs)
  (let ((cloned (sicl-mir:clone-instruction instruction)))
    (change-class instruction new-class
		  :inputs inputs
		  :outputs outputs
		  :successors (list cloned))
    (values instruction cloned)))

(defvar *linkage-vector-lexical*)
(defvar *code-object-lexical*)
(defvar *return-address-lexical*)
(defvar *static-environment-lexical*)

(defgeneric convert-instruction (instruction))

(defmethod convert-instruction (instruction)
  (declare (ignore instruction))
  nil)

(defun convert-instruction-graph (initial-instruction)
  (let ((table (make-hash-table :test #'eq))
	(*linkage-vector-lexical* (sicl-mir:new-temporary))
	(*code-object-lexical* (sicl-mir:new-temporary))
	(*return-address-lexical* (sicl-mir:new-temporary))
	(*static-environment-lexical* (sicl-mir:new-temporary))
	(all-instructions '()))
    ;; Start by collecting all the instructions to be processed, so as
    ;; to avoid converting an instruction twice as the instruction
    ;; graph changes.
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (push instruction all-instructions)
		 (mapc #'traverse (sicl-mir:successors instruction)))))
      (traverse initial-instruction))
    ;; Now convert all instructions previously collected.
    (mapc #'convert-instruction all-instructions)))

(defmethod sicl-program:convert-to-lir ((backend backend-arm) initial-instruction)
  (convert-instruction-graph initial-instruction))

;;; FIXME: check the constants
(defmethod convert-instruction ((instruction sicl-mir:enter-instruction))
  (setf (sicl-mir:outputs instruction)
	(list (aref *registers* 0)
	      (aref *registers* 1)
	      (aref *registers* 2)
	      (aref *registers* 3)
	      (aref *registers* 9)
	      (aref *registers* 12)
	      (aref *registers* 14)))
  ;; Generate code for moving the retrurn address to a lexical
  ;; variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 14)
	       *return-address-lexical*
	       (car (sicl-mir:successors instruction)))))
  ;; Generate code for moving the static environment argument to a
  ;; lexical variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 3)
	       *code-object-lexical*
	       (car (sicl-mir:successors instruction)))))
  ;; Generate code for moving the linkage vector argument to a lexical
  ;; variable.
  (setf (sicl-mir:successors instruction)
	(list (sicl-mir:make-assignment-instruction
	       (aref *registers* 12)
	       *linkage-vector-lexical*
	       (car (sicl-mir:successors instruction))))))

(defmethod convert-instruction ((instruction sicl-mir:get-arg-instruction))
  (let* ((input (car (sicl-mir:inputs instruction)))
	 (value (sicl-mir:value input)))
    (when (and (typep input 'sicl-mir:immediate-input) (<= value 8))
      (setf (sicl-mir:inputs instruction)
	    (list (aref *registers* (/ value 4)))))))

(defmethod convert-instruction ((instruction sicl-mir:funcall-instruction))
  (let ((inputs (sicl-mir:inputs instruction)))
    ;; Insert three assignment instructions preceding the
    ;; FUNCALL-INSTRUCTION, assigning lexical variables to the
    ;; physical registers used to store the corresponding arguments,
    ;; and replace the corresponding argument in the
    ;; FUNCALL-INSTRUCTION by the register.
    (loop for rest on (cdr inputs)
	  for i from 0 below 3
	  do (setf instruction
		   (nth-value 1 (add-instruction-before-instruction
				 instruction
				 'sicl-mir:assignment-instruction
				 (list (car rest))
				 (list (aref *registers* i)))))
	     (setf (car rest) (aref *registers* i)))
    ;; Insert instructions for loading the static environment and the
    ;; linkage vector of the callee into appropriate registers.
    (setf instruction
	  (nth-value 1 (add-instruction-before-instruction
			instruction
			'sicl-mir:load-static-env-instruction
			(list (car inputs))
			(list (aref *registers* 3)))))
    (setf instruction
	  (nth-value 1 (add-instruction-before-instruction
			instruction
			'sicl-mir:load-linkage-vector-instruction
			(list (car inputs))
			(list (aref *registers* 12)))))
    ;; Add the static environment and linkage vector registers as
    ;; input to the funcall instruction, right after the callee input
    ;; itself.
    (setf (cdr inputs)
	  (list* (aref *registers* 3) (aref *registers* 12) (cdr inputs)))
    ;; Indicate that the FUNCALL-INSTRUCTION returns values in some
    ;; registers, and trash some others by adding those registers as
    ;; outputs to the FUNCALL-INSTRUCTION.
    (setf (sicl-mir:outputs instruction)
	  (list (aref *registers* 0)
		(aref *registers* 1)
		(aref *registers* 2)
		(aref *registers* 3)
		(aref *registers* 14)))))

(defmethod convert-instruction ((instruction sicl-mir:tailcall-instruction))
  (let ((inputs (sicl-mir:inputs instruction)))
    ;; Insert three assignment instructions preceding the
    ;; FUNCALL-INSTRUCTION, assigning lexical variables to the
    ;; physical registers used to store the corresponding arguments,
    ;; and replace the corresponding argument in the
    ;; FUNCALL-INSTRUCTION by the register.
    (loop for rest on (cdr inputs)
	  for i from 0 below 3
	  do (setf instruction
		   (nth-value 1 (add-instruction-before-instruction
				 instruction
				 'sicl-mir:assignment-instruction
				 (list (car rest))
				 (list (aref *registers* i)))))
	     (setf (car rest) (aref *registers* i)))
    ;; Insert instructions for loading the static environment and the
    ;; linkage vector of the callee into appropriate registers.
    (setf instruction
	  (nth-value 1 (add-instruction-before-instruction
			instruction
			'sicl-mir:load-static-env-instruction
			(list (car inputs))
			(list (aref *registers* 3)))))
    (setf instruction
	  (nth-value 1 (add-instruction-before-instruction
			instruction
			'sicl-mir:load-linkage-vector-instruction
			(list (car inputs))
			(list (aref *registers* 12)))))
    ;; Add the static environment and linkage vector registers as
    ;; input to the funcall instruction, right after the callee input
    ;; itself.
    (setf (cdr inputs)
	  (list* (aref *registers* 3) (aref *registers* 12) (cdr inputs)))))

;;; For the ENCLOSE-INSTRUCTION, we just add the static environment
;;; and the linkage-vector as inputs to the instruction so that the
;;; register allocator can do its thing.
(defmethod convert-instruction ((instruction sicl-mir:enclose-instruction))
  (convert-instruction-graph (car (sicl-mir:inputs instruction)))
  (push *static-environment-lexical* (sicl-mir:inputs instruction))
  (push *linkage-vector-lexical* (sicl-mir:inputs instruction)))

(defmethod convert-instruction ((instruction sicl-mir:get-values-instruction))
  (loop for rest on (sicl-mir:outputs instruction)
	for i from 0 below 4
	do (setf (sicl-mir:successors instruction)
		 (list (sicl-mir:make-assignment-instruction
			(aref *registers* i)
			(car rest)
			(car (sicl-mir:successors instruction)))))
	   (setf (car rest) (aref *registers* i))))

(defmethod convert-instruction ((instruction sicl-mir:return-instruction))
  (loop for rest on (sicl-mir:inputs instruction)
	for i from 0 below 4
	do (setf instruction
		 (nth-value 1 (add-instruction-before-instruction
			       instruction
			       'sicl-mir:assignment-instruction
			       (list (car rest))
			       (list (aref *registers* i)))))
	   (setf (car rest) (aref *registers* i)))
  (push *return-address-lexical* (sicl-mir:inputs instruction)))

(defmethod convert-instruction
    ((instruction sicl-mir:load-constant-instruction))
  (push *linkage-vector-lexical* (sicl-mir:inputs instruction)))

(defmethod convert-instruction
    ((instruction sicl-mir:load-global-instruction))
  (push *linkage-vector-lexical* (sicl-mir:inputs instruction)))
