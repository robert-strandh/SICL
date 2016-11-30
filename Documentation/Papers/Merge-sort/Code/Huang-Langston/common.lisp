(cl:in-package #:huang-langston)

;;; Exchange two blocks of length LENGTH.  The first block starts at
;;; index START1 and the second block starts at index START2.  Blocks
;;; do not overlap.
(defun blockswap (vector start1 start2 length)
  (loop for i from start1
	for j from start2
	repeat length
	do (rotatef (aref vector i) (aref vector j))))

;;; Reverse the elements of a block of length LENGTH starting at index
;;; START.
(defun reverse-block (vector start length)
  (loop for i from start
	for j downfrom (+ start length -1)
	repeat (floor length 2)
	do (rotatef (aref vector i) (aref vector j))))

;;; Rotate a block with LENGTH elements starting at index START by
;;; AMOUNT places to the left.  Let PREFIX be the AMOUNT elements
;;; starting at START and the SUFFIX be the remaining elements.  The
;;; method used is to reverse the prefix and the suffix and then to
;;; reverse the entire block.
(defun rotate-block (vector start length amount)
  (reverse-block vector start amount)
  (reverse-block vector (+ start amount) length)
  (reverse-block vector start length))
