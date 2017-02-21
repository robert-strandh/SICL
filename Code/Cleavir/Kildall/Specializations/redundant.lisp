;;; THIS FILE IS NOT CURRENTLY INCORPORATED INTO ANY CLEAVIR SYSTEM

(defpackage #:cleavir-kildall-redundant
  (:use #:cl)
  (:export #:eliminate-superfluous-temporaries))

(in-package #:cleavir-kildall-redundant)

(defclass redundant-traverse
    (cleavir-kildall:forward-spread-traverse)
  ())

;;;; Pools are sets of equivalence classes for inputs
;;;; (as in, locations but also constants) that have the same value

;;;; That would be the ideal, but for this to have operational
;;;; effect we need to track the least recent assignment so that
;;;; the graph can be altered. So the equivalence classes are
;;;; actually sequences (lists) where the cars are more recent.

(defun make-seq (&rest objects)
  objects)

(defun in-seq-p (object seq) (find object seq))

;;; cl:union/intersection don't guarantee order.
(defun seq-union (seq1 seq2)
  (dolist (e seq1) (pushnew e seq2))
  seq2)

(defun seq-intersection (seq1 seq2)
  (remove-if-not (lambda (e) (in-seq-p e seq2)) seq1))

;;; order not important here.
(defun subseqp (seq1 seq2)
  (cl:subsetp seq1 seq2))

;;; doesn't check to see if it's there already
(defun add-to-seq (object seq)
  (cons object seq))

;;; fine to call if the seq doesn't contain the element
(defun remove-from-seq (object seq)
  (remove object seq))

;;; kildall methods

(defmethod cleavir-kildall:entry-pool ((s redundant-traverse) instruction)
  (check-type instruction cleavir-ir:enter-instruction)
  (make-seq))

;;; intersection of sets of equivalence classes... ow.
;;; This is what Kildall has. It's not efficient.
(defmethod cleavir-kildall:pool-meet ((s redundant-traverse) p1 p2)
  ;; actually don't need ordering for the let variables.
  (let* ((p1-union (reduce #'seq-union p1))
	 (p2-union (reduce #'seq-union p2))
	 (intersect (seq-intersection p1-union p2-union)))
    (loop for input in intersect
	  unless (find intersect result :test #'in-seq-p)
	    ;; but we do need order here
	    collect (seq-intersection
		     (find input p1 :test #'in-seq-p)
		     (find input p2 :test #'in-seq-p))
	      into result
	  finally (return result))))

;;; also not efficient, but better than kildall, probably.
(defmethod cleavir-kildall:pool<= ((s redundant-traverse) p1 p2)
  (every (lambda (class1) (find class1 p2 :test #'subseqp)) p1))

(defmethod cleavir-kildall:transfer ((s redundant-traverse) i pool)
  ;; Add any constant inputs that aren't already in a class.
  (dolist (in (cleavir-ir:inputs i))
    (unless (or (cleavir-ir:variable-p in)
		(find in pool :test #'in-seq-p))
      (setf pool (cons (make-seq in) pool))))
  ;; For each output: remove the class with that output in it, if
  ;; there is one, because it (probably) has a new value now.
  ;; Then append a new singleton seq with that output to the pool.
  (dolist (out (cleavir-ir:outputs i))
    (setf pool
	  (mapcar (lambda (class) (remove-from-seq out class))
		  pool)))
  (dolist (out (cleavir-ir:outputs i))
    (setf pool (cons (make-seq out) pool)))
  pool)

(defmethod cleavir-kildall:transfer
    ((s redundant-traverse)
     (instruction cleavir-ir:assignment-instruction)
     pool)
  ;; If the input is in a class, as it will be if it's a variable,
  ;; put the output in that class.
  ;; If there is no class (for constants) make one for both.
  ;; Put the output in that pool.
  (let ((in (first (cleavir-ir:inputs instruction)))
	(out (first (cleavir-ir:outputs instruction))))
    (let ((class (find in pool :test #'in-seq-p)))
      (if class
	  (cons (add-to-seq out class) (remove class pool))
	  (cons (make-seq out in) pool)))))

(defun redundancies (initial-instruction)
  (let ((traverse (make-instance 'redundant-traverse)))
    (cleavir-kildall:kildall traverse initial-instruction)))

(defun reassign (initial-instruction redundancies)
  (cleavir-ir:map-instructions-locally
   (lambda (instruction)
     (loop with classes = (gethash instruction redundancies)
	   for input-cons on (cleavir-ir:inputs instruction)
	   for input = (car input-cons)
	   do (let ((class (find input classes :test #'find)))
		(when class
		  (setf (car input-cons) (car (last class)))))))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))

;;; Could be expanded to remove all non-side-effectful instructions
;;; that output to no-longer-used locations, but I don't think that
;;; situation actually arises.
(defun remove-assignments (initial-instruction)
  (let (death)
    (cleavir-ir:map-instructions-locally
     (lambda (instruction)
       (when (and
	      (typep instruction 'cleavir-ir:assignment-instruction)
	      (null (cleavir-ir:using-instructions (first (cleavir-ir:outputs instruction)))))
	 (push instruction death)))
     initial-instruction)
    (mapc #'cleavir-ir:delete-instruction death))
  nil)

(defun eliminate-superfluous-temporaries (initial-instruction)
  (reassign initial-instruction (redundancies initial-instruction))
  (remove-assignments initial-instruction)
  initial-instruction)
