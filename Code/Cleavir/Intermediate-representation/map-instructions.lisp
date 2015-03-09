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
		   (if (null instructions-to-process)
		       (setf instructions-to-process (list (code instruction)))
		       (setf (rest (last instructions-to-process))
			     (list (code instruction))))
		   (setf (gethash instruction visited-instructions) t))
		 ;; For each successor of the current instruction, check
		 ;; whether it has already been visited, and if not,
		 ;; make sure it gets processed later by pushing it onto
		 ;; the list of instructions yet to process.  By putting
		 ;; the successors first on the list, the traversal
		 ;; becomes depth-first.
		 (loop for successor in (successors instruction)
		       unless (gethash successor visited-instructions)
			 do (register-if-unvisited successor)))))))
