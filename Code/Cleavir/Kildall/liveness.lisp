;;; THIS FILE IS NOT CURRENTLY INCORPORATED INTO ANY CLEAVIR SYSTEM

(defpackage #:cleavir-kildall-liveness
  (:use #:cl)
  (:export #:liveness #:live-before #:live-after))

(in-package #:cleavir-kildall-liveness)

;;; class for holding liveness information.
(defclass liveness ()
  ((%before :initarg :before :reader before)
   (%after :initarg :after :reader after)))

;;; traverse/domain for kildall.
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

(defun compute-liveness-after (initial-instruction)
  (let ((traverse (make-instance 'liveness-traverse)))
    (cleavir-kildall:kildall traverse initial-instruction)))

(defun liveness (initial-instruction)
  (let ((after (compute-liveness-after initial-instruction))
	(before (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (setf (gethash i before)
	     (reduce #'union
		     (cleavir-ir:predecessors i)
		     :key (lambda (p) (gethash p after))
		     :initial-value nil)))
     initial-instruction)
    (make-instance 'liveness :before before :after after)))

(defun live-before (liveness node)
  (gethash node (before liveness)))

(defun live-after (liveness node)
  (gethash node (after liveness)))
