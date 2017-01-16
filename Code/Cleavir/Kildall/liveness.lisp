(defpackage #:cleavir-kildall-liveness
  (:use #:cl)
  (:export #:liveness-after))

(in-package #:cleavir-kildall-liveness)

;;;; Implements liveness-after.

(defclass liveness-traverse
    (cleavir-kildall:reverse-spread-traverse) ())

;;;; Meet is set union. 1 is the empty set (list). 0 doesn't appear
;;;; A <= B is B subset A.
;;;; sets (as lists) of locations are "these locations are live"

(defun variable-p (input)
  (typep input '(or cleavir-ir:lexical-location
		 cleavir-ir:values-location)))

(defun variable-inputs (instruction)
  (remove-if-not #'variable-p (cleavir-ir:inputs instruction)))

(defmethod cleavir-kildall:entry-pool
    ((s liveness-traverse) instruction)
  (declare (ignore instruction))
  nil)

(defmethod cleavir-kildall:pool<= ((s liveness-traverse) p1 p2)
  (subsetp p2 p1))

(defmethod cleavir-kildall:pool-meet ((s liveness-traverse) p1 p2)
  (union p1 p2))

(defmethod cleavir-kildall:transfer
    ((s liveness-traverse) instruction pool)
  (union (set-difference pool (cleavir-ir:outputs instruction))
	 (variable-inputs instruction)))

(defun liveness-after (initial-instruction)
  (let ((traverse (make-instance 'liveness-traverse)))
    (cleavir-kildall:kildall traverse initial-instruction)))
