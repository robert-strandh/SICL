(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS-ARBITRARY-ORDER
;;;
;;; Traverse an instruction graph in some arbitrary order.
;;; INITIAL-INSTRUCTION is the root of the instruction graph.
;;; FUNCTION will be called exactly once for each instruction in the
;;; instruction graph, and it will be called with that instruction as
;;; the only argument.

;;; We create a meter that will keep track of the execution time and
;;; the number of instructions that were processed.
(defparameter *map-instructions-arbitrary-order-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "MAP-INSTRUCTIONS-ARBITRARY-ORDER"))

(defun map-instructions-arbitrary-order (function initial-instruction)
  (cleavir-meter:with-meter (m *map-instructions-arbitrary-order-meter*)
    (let ((visited-instructions (make-hash-table :test #'eq))
	  (instructions-to-process '()))
      (flet ((register-if-unvisited (instruction)
	       (unless (gethash instruction visited-instructions)
		 (cleavir-meter:increment-size m)
		 (setf (gethash instruction visited-instructions) t)
		 (push instruction instructions-to-process))))
	(register-if-unvisited initial-instruction)
	(loop until (null instructions-to-process)
	      do (let ((instruction (pop instructions-to-process)))
		   (funcall function instruction)
		   (when (typep instruction 'cleavir-ir:enclose-instruction)
		     ;; When the instruction is an ENCLOSE-INSTRUCTION,
		     ;; we must also account for the CODE slot of the
		     ;; instruction, because it contains the
		     ;; ENTER-INSTRUCTION of a nested function.
		     (register-if-unvisited (code instruction)))
		   ;; For each successor of the current instruction,
		   ;; register it so that it will be processed
		   ;; ultimately, unless, of course, it has already been
		   ;; processed.
		   (loop for successor in (successors instruction)
			 do (register-if-unvisited successor))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS.
;;;
;;; Call FUNCTION on each instruction of an instruction graph the root
;;; of which is INITIAL-INSTRUCTION.  In this version of the mapping
;;; function, we traverse the instruction graph depth-first, and we
;;; make sure that FUNCTION is called on every instruction reachable
;;; from the ENTER-INSTRUCTION of a particular function before it is
;;; called on the instructions of any nested function.
;;;
;;; It is still possible for some instructions of a particular
;;; function to be processed only as part of processing a nested
;;; function.  Consider the following code snippet:
;;;
;;;   (tagbody
;;;       (ff (lambda () (go a)))
;;;       (go out)
;;;     a
;;;       (print 234)
;;;     out)
;;;
;;; Here the form (print 234) is part of the outermost function, but
;;; it can not be reached from the initial instruction of that
;;; function.  It will therefore be processed only as part of
;;; processing the nested function (lambda () (go a)).
;;;
;;; We traverse the graph by using an explicit stack (in the form of
;;; an ordinary list) of instruction to process, rather than using the
;;; control stack in a recursive traversal function.  The reason for
;;; doing it this way is that the control stack of many
;;; implementations can be relatively small, and if the instruction
;;; graph is big, then the control stack can easily be exhausted.

(defun map-instructions (function initial-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
	(instructions-to-process '()))
    (flet ((register-if-unvisited (instruction)
	     (unless (gethash instruction visited-instructions)
	       (setf (gethash instruction visited-instructions) t)
	       (push instruction instructions-to-process))))
      (register-if-unvisited initial-instruction)
      (loop until (null instructions-to-process)
	    do (let ((instruction (pop instructions-to-process)))
		 (funcall function instruction)
		 (when (typep instruction 'cleavir-ir:enclose-instruction)
		   ;; When the instruction is an ENCLOSE-INSTRUCTION,
		   ;; we must also account for the CODE slot of the
		   ;; instruction, because it contains the
		   ;; ENTER-INSTRUCTION of a nested function.  We put
		   ;; this ENTER-INSTRUCTION last on the list of
		   ;; instructions to process.  By doing it this way,
		   ;; we make sure that all the instructions that are
		   ;; reachable from the initial ENTER-INSTRUCTION of
		   ;; a particular function are processed before any
		   ;; instruction of a nested function is processed.
		   ;; Since we haven't processed this
		   ;; ENCLOSE-INSTRUCTION before, and since each
		   ;; nested ENTER-INSTRUCTION is the value of the
		   ;; CODE slot of a single ENCLOSE-INSTRUCTION, we
		   ;; know that we haven't yet visited the nested
		   ;; ENTER-INSTRUCTION.  Therefore, no test is
		   ;; necessary for that situation.
		   (let ((code (code instruction)))
		     (if (null instructions-to-process)
			 (setf instructions-to-process (list code))
			 (setf (rest (last instructions-to-process))
			       (list code)))
		     (setf (gethash code visited-instructions) t)))
		 ;; For each successor of the current instruction, check
		 ;; whether it has already been visited, and if not,
		 ;; make sure it gets processed later by pushing it onto
		 ;; the list of instructions yet to process.  By putting
		 ;; the successors first on the list, the traversal
		 ;; becomes depth-first.
		 (loop for successor in (successors instruction)
		       do (register-if-unvisited successor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS-WITH-OWNER.
;;;
;;; In this variation of MAP-INSTRUCTIONS, FUNCTION is called with two
;;; arguments, namely the instruction and the owner of the
;;; instruction.
;;;
;;; We accomplish this variation by modifying the list of instructions
;;; to process so that it holds pairs (INSTRUCTION . OWNER).
;;; Otherwise, the logic is the same as with MAP-INSTRUCTIONS.

(defun map-instructions-with-owner (function initial-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
	(instructions-to-process '()))
    (flet ((register-if-unvisited (instruction owner)
	     (unless (gethash instruction visited-instructions)
	       (setf (gethash instruction visited-instructions) t)
	       (push (cons instruction owner)
		     instructions-to-process))))
      (register-if-unvisited initial-instruction initial-instruction)
      (loop until (null instructions-to-process)
	    do (destructuring-bind (instruction . owner)
		   (pop instructions-to-process)
		 (funcall function instruction owner)
		 (cond ((typep instruction 'cleavir-ir:enclose-instruction)
			(let ((code (code instruction)))
			  (if (null instructions-to-process)
			      (setf instructions-to-process
				    (list (cons code code)))
			      (setf (rest (last instructions-to-process))
				    (list (cons code code))))
			  (setf (gethash code visited-instructions) t))
			(register-if-unvisited
			 (first (successors instruction))
			 ;; The owner of the successor of the
			 ;; ENCLOSE-INSTRUCTION is the same as the
			 ;; owner of the enclose-instruction itself.
			 owner))
		       ((typep instruction 'cleavir-ir:unwind-instruction)
			(register-if-unvisited
			 (first (successors instruction))
			 ;; The owner of the successor of the
			 ;; UNWIND-INSTRUCTION is the INVOCATION of
			 ;; the UNWIND-INSTRUCTION.
			 (invocation instruction)))
		       (t
			(loop for successor in (successors instruction)
			      do (register-if-unvisited
				  successor
				  ;; For all other instructions the
				  ;; owner of each successor is the
				  ;; same as the owner of the
				  ;; instruction itself.
				  owner)))))))))
