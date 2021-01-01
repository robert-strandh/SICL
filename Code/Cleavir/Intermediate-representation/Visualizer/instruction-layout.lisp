(cl:in-package #:cleavir-ir-visualizer)

;;; This table is created for each function.  It maps an instruction
;;; to T if and only if it has already been considered for layout.
(defvar *instruction-table*)

;;; Take a layer of instructions that may contain instructions that
;;; have already been considered for layout, and that may contain
;;; duplicates and return a filtered version of it.  The filtered
;;; version contains no duplicates and it does not contain any
;;; instructions that have already been considered for layout, nor
;;; does it include instructions that are in the REST of
;;; REMAINING-PATH because those are going to be included in a
;;; subsequent layer
(defun filter-instructions (layer remaining-path)
  (remove-duplicates
   (loop for instruction in layer
         unless (or (gethash instruction *instruction-table*)
                    (member instruction (cdr remaining-path)))
           collect instruction)))

(defun populate-table (instructions)
  (loop for instruction in instructions
        do (setf (gethash instruction *instruction-table*) t)))

(defun next-instruction-layer (layer)
  (loop for instruction in layer
        append (cleavir-ir:successors instruction)))

(defun compute-layers (enter-instruction)
  (let ((longest-path (find-longest-simple-path enter-instruction)))
    (loop with layer = (list enter-instruction)
          for remaining-path on longest-path
          for result = (filter-instructions layer remaining-path)
          collect result
          do (populate-table result)
             (setf layer (next-instruction-layer layer)))))

(defun layout-layer (instructions hpos vpos)
  (loop for node in instructions
        for dx = (+ (floor (round *base-width*) 2) 10)
          then (+ dx horizontal-column-separation)
        do (setf (gethash node *instruction-table*) t)
           (setf (instruction-position node)
                 (cons (+ hpos dx) vpos))))

(defun layout-function (enter-instruction hpos vpos)
  (let ((*instruction-table* (make-hash-table :test #'eq)))
    (loop for layer in (compute-layers enter-instruction)
          for dy from 20 by vertical-node-separation
          do (layout-layer layer hpos (+ vpos dy)))))

;;; Compute the width and the height of a function
(defun compute-function-dimensions (enter-instruction pane)
  (let ((*instruction-table* (make-hash-table :test #'eq)))
    (loop for layer in (compute-layers enter-instruction)
          for vpos from 20 by (* 3 (node-height pane))
          maximize (compute-layer-width layer pane) into width
          finally (return (values width vpos)))))
