(in-package #:cleavir-kildall)

;;;; Dictionaries are the ultimate result of the algorithm and form
;;;; maps from instructions to pools. They're hash tables. The end

(declaim (inline make-dictionary
		 instruction-pool (setf instruction-pool)))

(defun make-dictionary ()
  (make-hash-table :test 'eq))

;;; note that the present-p value is actually needed.
(defun instruction-pool (instruction dictionary)
  (gethash instruction dictionary))
(defun (setf instruction-pool) (pool instruction dictionary)
  (setf (gethash instruction dictionary) pool))
