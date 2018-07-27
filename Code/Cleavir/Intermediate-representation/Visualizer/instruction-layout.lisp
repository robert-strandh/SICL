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

(defun compute-layers (enter-instruction)
  (loop with longest-path = (find-longest-simple-path enter-instruction)
	for instruction-on-longest-path in longest-path
	for layer = (list enter-instruction)
	  then (next-instruction-layer layer instruction-on-longest-path)
	until (null layer)
	collect layer))

(defun layout-function (enter-instruction hpos vpos pane)
  (let ((*instruction-table* (make-hash-table :test #'eq)))
    (loop with longest-path = (find-longest-simple-path enter-instruction)
          for instruction-on-longest-path in longest-path
          for layer = (list enter-instruction)
            then (next-instruction-layer layer instruction-on-longest-path)
          for dy from 20 by (* 3 (node-height pane))
          until (null layer)
          do (loop for node in layer
                   for width = (node-width node pane)
                   for dx = (+ (floor width 2) 10)
                     then (+ dx width *horizontal-node-separation*)
                   do (setf (gethash node *instruction-table*) t)
                      (setf (instruction-position node)
                            (cons (+ hpos dx) (+ vpos dy)))))))
