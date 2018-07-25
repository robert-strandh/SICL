(cl:in-package #:cleavir-ir-visualizer)

;;; This table is created for each function.  It maps an instruction
;;; to T if and only if it has already been considered for layout.
(defvar *instruction-table*)

;;; Take a layer of instructions that may contain instructions that
;;; have already been considered for layout, and that may contain
;;; duplicates and return a filtered version of it.  The filtered
;;; version contains no duplicates and it does not contain any
;;; instructions that have already been considered for layout, except
;;; that if one of the instructions in the layer is the same as
;;; INSTRUCTION-ON-LONGEST-PATH, then it is left included.
(defun filter-instructions (layer instruction-on-longest-path)
  (remove-duplicates
   (loop for instruction in layer
         when (or (not (gethash instruction *instruction-table*))
                  (eq instruction instruction-on-longest-path))
           collect instruction)))

(defun next-instruction-layer (layer instruction-on-longest-path)
  (let ((layer (loop for instruction in layer
                     append (cleavir-ir:successors instruction))))
    (filter-instructions layer instruction-on-longest-path)))
