(cl:in-package #:cleavir-ir-visualizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define some abstractions for managing instruction positions.

;;; Given an instruction as the key, this table contains a CONS of the
;;; horizontal position and the vertical position for that
;;; instruction.  This variable must be bound at the beginning of the
;;; visualization.
(defvar *instruction-position-table*)

;;; Return the vertical position and the horizontal position of
;;; INSTRUCTION as two values.  If either of them has not been set,
;;; then signal an error.
(defun instruction-position (instruction)
  (let ((entry (gethash instruction *instruction-position-table*)))
    (cond ((null entry)
           (error "No position has been assigned to ~s" instruction))
          ((null (car entry))
           (error "No horizontal position has been assigned to ~s" instruction))
          ((null (cdr entry))
           (error "No vertical position has been assigned to ~s" instruction))
          (t
           (values (car entry) (cdr entry))))))

;;; Return the horizontal position of INSTRUCTION.  If the horizontal
;;; position has not been set, then signal an error.
(defun instruction-horizontal-position (instruction)
  (let ((entry (gethash instruction *instruction-position-table*)))
    (cond ((or (null entry) (null (car entry)))
           (error "No horizontal position has been assigned to ~s" instruction))
          (t
           (car entry)))))

;;; Return the vertical position of INSTRUCTION.  If the vertical
;;; position has not been set, then signal an error.
(defun instruction-vertical-position (instruction)
  (let ((entry (gethash instruction *instruction-position-table*)))
    (cond ((or (null entry) (null (cdr entry)))
           (error "No vertical position has been assigned to ~s" instruction))
          (t
           (cdr entry)))))
