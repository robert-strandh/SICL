(cl:in-package #:cleavir-lexical-depth)

(defun map-instructions (function enter-instruction)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (funcall function instruction)
		 (mapc #'traverse (cleavir-mir:successors instruction)))))
      (traverse enter-instruction))))

