(cl:in-package #:cleavir-hir-transformations)

;;;; Type inference is done at the HIR level, after inlining.  We have
;;;; yet to determine the exact protocol for type inference.  The main
;;;; difficulty is that the type system may vary between
;;;; implementations.  We therefore have to find a way to both
;;;; customize type inference for each implementation, and propose
;;;; helpful defaults.

(defun type-inference (initial-instruction)
  (declare (ignore initial-instruction))
  nil)

;;  LocalWords:  inlining
