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

(defun map-instructions-arbitrary-order (function initial-instruction)
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
                   ;; ENTER-INSTRUCTION of a nested function.
                   (register-if-unvisited (code instruction)))
                 ;; For each successor of the current instruction,
                 ;; register it so that it will be processed
                 ;; ultimately, unless, of course, it has already been
                 ;; processed.
                 (loop for successor in (successors instruction)
                       do (register-if-unvisited successor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FILTER-INSTRUCTIONS
;;;
;;; Return, in some arbitrary order, a list of all instructions
;;; reachable from the root that satisfy a predicate.
;;; Sort of like REMOVE-IF-NOT.

(defun filter-instructions (predicate initial-instruction)
  (let (result)
    (map-instructions-arbitrary-order
     (lambda (instruction)
       (when (funcall predicate instruction)
	 (push instruction result)))
     initial-instruction)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function INSTRUCTIONS-OF-TYPE
;;;
;;; Return, in some arbitrary order, a list of all instructions
;;; reachable from the root that are of some type.
;;; This is useful for many transformations.

(defun instructions-of-type (initial-instruction type)
  (filter-instructions (lambda (i) (typep i type))
                       initial-instruction))
