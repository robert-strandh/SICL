(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAP-LOCAL-INSTRUCTIONS
;;;
;;; Traverse an instruction graph in depth first order.  The traversal
;;; starts from an ENTER-INSTRUCTION, and does not descend into
;;; enclosed functions; thus all instructions mapped have the input
;;; ENTER as their owner, and all reachable instructions with the
;;; ENTER as their owner are mapped.

(defun map-local-instructions (function enter-instruction)
  (let ((visited-instructions (make-hash-table :test #'eq))
        (instructions-to-process '()))
    (flet ((register-if-unvisited (instruction)
             (unless (gethash instruction visited-instructions)
               (setf (gethash instruction visited-instructions) t)
               (push instruction instructions-to-process))))
      (register-if-unvisited enter-instruction)
      (loop until (null instructions-to-process)
            do (let ((instruction (pop instructions-to-process)))
                 (funcall function instruction)
                 (mapc #'register-if-unvisited
                       (cleavir-ir:successors instruction)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FILTER-LOCAL-INSTRUCTIONS
;;;
;;; Return, in some arbitrary order, a list of all local instructions
;;; that satisfy a predicate.  Sort of like REMOVE-IF-NOT.

(defun filter-local-instructions (predicate enter-instruction)
  (let (result)
    (map-local-instructions
     (lambda (instruction)
       (when (funcall predicate instruction)
         (push instruction result)))
     enter-instruction)
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function LOCAL-INSTRUCTIONS-OF-TYPE
;;;
;;; Return, in some arbitrary order, a list of all local instructions
;;; that are of some type.  This is useful for many transformations.

(defun local-instructions-of-type (enter-instruction type)
  (filter-local-instructions (lambda (i) (typep i type))
                             enter-instruction))

(define-compiler-macro local-instructions-of-type (&whole form initial-instruction type)
  (if (constantp type)
      `(filter-local-instructions (lambda (i) (typep i ',(eval type))) ,initial-instruction)
      form))
