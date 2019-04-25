(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SET-PREDECESSORS.
;;;
;;; Given an instruction graph in which the list of SUCCESSORS of each
;;; instruction is correct, but the list of PREDECESSORS might not be,
;;; we set the predecessors from the successors.
;;; 
;;; An existing list of predecessors might be arbitrarily reordered. 

(defun set-predecessors (initial-instruction)
  ;; First, set the list of predecessors of each instruction to the
  ;; empty list, just in case there are some spurious predecessors on
  ;; any instruction.
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (setf (predecessors instruction) '()))
   initial-instruction)
  ;; Next add each instruction as a predecessor to each of its
  ;; successors.
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for successor in (successors instruction)
           do (push instruction (predecessors successor))))
   initial-instruction))
