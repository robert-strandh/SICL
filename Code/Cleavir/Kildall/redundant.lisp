(defpackage #:cleavir-kildall-redundant
  (:use #:cl))

(in-package #:cleavir-kildall-redundant)

;;;; each pool is the sets of equivalence classes for variables
;;;; before that instruction.

(defclass redundant-traverse
    (cleavir-kildall:forward-single-traverse)
  ())

;;;; pools are sets of equivalence classes,
;;;; represented as lists of lists.

(defmethod cleavir-kildall:entry-pool ((s redundant-traverse) instruction)
  (check-type instruction cleavir-ir:enter-instruction)
  nil)

;;; intersection of sets of equivalence classes... ow.
(defmethod cleavir-kildall:pool-meet ((s redundant-traverse) p1 p2)
  (loop
    with p2-copy = (copy-list p2) ; so we can delete stuff
    for class in p1
    when (let ((other-class
		 (loop
		   named search
		   for e in class
		   do (loop
			for oc in p2-copy
			when (find e oc)
			  do (setf p2-copy (delete oc p2-copy))
			     (return-from search oc)))))
	   (intersection class other-class))
      collect it))

(defmethod cleavir-kildall:pool<= ((s redundant-traverse) p1 p2)
  (every (lambda (class) (find class p2 :test #'subsetp)) p1))

(defmethod cleavir-kildall:transfer ((s redundant-traverse) i pool)
  (append (mapcar #'list (cleavir-ir:outputs i)) pool))

(defmethod cleavir-kildall:transfer
    ((s redundant-traverse)
     (instruction cleavir-ir:enter-instruction)
     pool)
  (declare (ignore pool))
  (mapcar #'list (cleavir-ir:inputs instruction)))

(defmethod cleavir-kildall:transfer
    ((s redundant-traverse)
     (instruction cleavir-ir:assignment-instruction)
     pool)
  (let ((in (first (cleavir-ir:inputs instruction)))
	(out (first (cleavir-ir:outputs instruction))))
    (let ((class (find in pool :test #'find)))
      (if class
	  (cons (cons out class) (remove class pool))
	  (cons (list out in) pool)))))

(defun redundancies (initial-instruction)
  (let ((traverse (make-instance 'redundant-traverse)))
    (cleavir-kildall:kildall traverse initial-instruction)))

(defun reassign (initial-instruction redundancies)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (instruction)
     (loop with classes = (gethash instruction redundancies)
	   for input-cons on (cleavir-ir:inputs instruction)
	   for input = (car input-cons)
	   do (let ((class (find input classes :test #'find)))
		(when class
		  (setf (car input-cons) (car (last class)))))))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))

(defun remove-assignments (initial-instruction)
  (let (death)
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and
	      (typep instruction 'cleavir-ir:assignment-instruction)
	      (null (cleavir-ir:using-instructions (first (cleavir-ir:outputs instruction)))))
	 (push instruction death)))
     initial-instruction)
    (mapc #'cleavir-ir:delete-instruction death)))
