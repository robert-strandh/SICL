(cl:in-package #:cleavir-ir)

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
		 (when (typep instruction 'enclose-instruction)
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
		 (cond ((typep instruction 'enclose-instruction)
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
		       ((typep instruction 'unwind-instruction)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS-BY/WITH-OWNER.
;;;
;;; This function calls FUNCTION with every instruction reachable from
;;; INITIAL-INSTRUCTION.
;;;
;;; Let's define a PARENT relationship between ENTER-INSTRUCTIONs.
;;; INITIAL-INSTRUCTION has no parent.  Any ENTER-INSTRUCTION (say A)
;;; other than INITIAL-INSTRUCTION is the CODE of some
;;; ENCLOSE-INSTRUCTION (say B).  The parent of A is then the owner of
;;; B.
;;;
;;; The things that characterizes MAP-INSTRUCTIONS-BY/WITH-OWNER is
;;; that it calls FUNCTION with the instructions in a particular order
;;; that can be characterized as follows: when FUNCTION is called on
;;; some instruction A, then it has already been called on EVERY
;;; INSTRUCTION B such that the owner of B is the parent of the owner
;;; of A.
;;;
;;; Why is this order important, one might ask?  Well, it can be used
;;; to compute ownership of DATA.  Recall that the owner of a datum is
;;; the MOST SENIOR (in terms of the PARENT relationship) of the
;;; owners of the instructions that refer to it.  So we can compute
;;; the owner ship of DATA by traversing the instructions with this
;;; function, attributing an owner to the data used by an instruction
;;; only if no owner has previously been attributed.
;;;
;;; One might also ask why this function is non-trivial to implement.
;;; Consider this code: (BLOCK NIL (FUNCALL (LAMBDA () (RETURN
;;; NIL)))).  This code contains a RETURN-INSTRUCTION with no
;;; successors (of course) and with an UNWIND-INSTRUCTION as its only
;;; predecessor.  In other words, the RETURN instruction is not
;;; reachable from its owner by following only predecessors and
;;; successors.  The only way to reach it is to go through a child.

(defun map-instructions-by/with-owner (function initial-instruction)
  (let (;; A key of this hash table is an ENTER-INSTRUCTION.  The
	;; corresponding value is a list of all the instructions owned
	;; by that ENTER-INSTRUCTION, including the ENTER-INSTRUCTION
	;; itself.
	(owned-instructions (make-hash-table :test #'eq))
	;; A key of this hash table is an ENTER-INSTRUCTION.  The
	;; corresponding value is the PARENT of that ENTER-INSTRUCTION
	;; provided the ENTER-INSTRUCTION has a parent
	(parents (make-hash-table :test #'eq)))
    (map-instructions-with-owner
     (lambda (instruction owner)
       ;; Add INSTRUCTION to the list of instructions owned by OWNER.
       (push instruction (gethash owner owned-instructions))
       (when (typep instruction 'cleavir-ir:enclose-instruction)
	 ;; The parent of the CODE of INSTRUCTION is the OWNER of
	 ;; INSTRUCTION.
	 (setf (gethash (cleavir-ir:code instruction) parents) owner)))
     initial-instruction)
    (let (;; This hash table is just a set of all the
	  ;; ENTER-INSTRUCTIONs that have been traversed
	  (traversed-owners (make-hash-table :test #'eq)))
      ;; We do a recursive traversal, but only between parents, so the
      ;; recursion depth is small.
      (labels ((traverse (owner)
		 (unless (gethash owner traversed-owners)
		   (let ((parent (gethash owner parents)))
		     (unless (null parent)
		       (traverse parent)))
		   (setf (gethash owner traversed-owners) t)
		   (loop for instruction in (gethash owner owned-instructions)
			 do (funcall function instruction owner)))))
	(loop for owner being each hash-key of owned-instructions
	      do (traverse owner))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS-BY-OWNER.

(defun map-instructions-by-owner (function initial-instruction)
  (map-instructions-by/with-owner
   (lambda (instruction owner)
     (declare (ignore owner))
     (funcall function instruction))
   initial-instruction))
