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

;;; We create a meter that will keep track of the execution time.
(defparameter *set-predecessors-meter*
  (make-instance 'cleavir-meter:size-meter
    :name "SET-PREDECESSORS"))

(defun set-predecessors (initial-instruction)
  (cleavir-meter:with-meter (m *set-predecessors-meter*)
    ;; First, set the list of predecessors of each instruction to the
    ;; empty list, just in case there are some spurious predecessors on
    ;; any instruction.
    (map-instructions-arbitrary-order
     (lambda (instruction)
       (cleavir-meter:increment-size m)
       (setf (predecessors instruction) '()))
     initial-instruction)
    ;; Next add each instruction as a predecessor to each of its
    ;; successors.
    (map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for successor in (successors instruction)
	     do (push instruction (predecessors successor))))
     initial-instruction)))
