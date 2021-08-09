(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-INSTRUCTIONS.
;;;
;;; Call FUNCTION on each instruction of an instruction graph the root
;;; of which is INITIAL-INSTRUCTION.  In this version of the mapping
;;; function, we traverse the instruction graph depth-first, and we
;;; make sure that FUNCTION is called on every instruction of a
;;; particular function before it is called on the instructions of
;;; any nested functions.
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
               (push instruction instructions-to-process)))
           (register-last-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (if (null instructions-to-process)
                   (setf instructions-to-process (list instruction))
                   (setf (rest (last instructions-to-process))
                         (list instruction))))))
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
                   (register-last-if-unvisited (code instruction)))
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
;;; Why is this important, one might ask?  Well, it can be used
;;; to compute ownership of DATA.  Recall that the owner of a datum is
;;; the MOST SENIOR (in terms of the PARENT relationship) of the
;;; owners of the instructions that refer to it.  So we can compute
;;; the ownership of DATA by traversing the instructions with this
;;; function, attributing an owner to the data used by an instruction
;;; only if no owner has previously been attributed.
;;;
;;; The internal logic is essentially identical to that of MAP-INSTRUCTIONS,
;;; but we track the "current owner" as well. Because enclose CODEs are
;;; added to the end of the worklist, we can just do one entire function at
;;; a time, tracking one owner.

(defun map-instructions-with-owner (function initial-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
        (instructions-to-process '())
        (current-owner initial-instruction))
    (flet ((register-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (push instruction instructions-to-process)))
           (register-last-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (if (null instructions-to-process)
                   (setf instructions-to-process (list instruction))
                   (setf (rest (last instructions-to-process))
                         (list instruction))))))
      (register-if-unvisited initial-instruction)
      (loop until (null instructions-to-process)
            do (let ((instruction (pop instructions-to-process)))
                 (when (typep instruction 'enter-instruction)
                   (setf current-owner instruction))
                 (funcall function instruction current-owner)
                 (when (typep instruction 'enclose-instruction)
                   (register-last-if-unvisited (code instruction)))
                 (loop for successor in (successors instruction)
                       do (register-if-unvisited successor)))))))
