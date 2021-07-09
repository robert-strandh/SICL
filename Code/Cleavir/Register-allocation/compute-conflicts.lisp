(in-package #:cleavir-register-allocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute a set of conflicts for the register allocator.  Recall
;;; that two variables generate a conflict when one is live at the
;;; point where the other is written to.  Furthermore, all items that
;;; are written to by the same instruction conflict with each other.
;;;
;;; We do not want multiple copies of some conflict.  We have to be
;;; careful because the relation is symmetric, so that if (L1 . L2) is
;;; a conflict in the set, we do not want to add (L2 . L1) because it
;;; is the same conflict.
;;;
;;; Conflicts are computed only for lexical variables.  Other types of
;;; locations are ignored.

(defun same-conflict-p (c1 c2)
  (or (and (eq (car c1) (car c2))
           (eq (cdr c1) (cdr c2)))
      (and (eq (car c1) (cdr c2))
           (eq (cdr c1) (car c2)))))

(defun conflicts-instruction (instruction liveness)
  (loop with outputs = (cleavir-ir:outputs instruction)
        for output in outputs
        append (when (typep output 'cleavir-ir:lexical-location)
                 (loop for live in (cleavir-liveness:live-after
                                    liveness instruction)
                       when (typep live 'cleavir-ir:lexical-location)
                         collect (cons output live)))))

(defun compute-conflicts (initial-instruction)
  (let ((conflicts '())
        (table (make-hash-table :test #'eq))
        (liveness (cleavir-liveness:liveness
                   initial-instruction
                   #'cleavir-ir:successors
                   #'cleavir-ir:predecessors
                   (lambda (instruction)
                     (remove-if-not
                      (lambda (input)
                        (typep input 'cleavir-ir:lexical-location))
                      (cleavir-ir:inputs instruction))) 
                   (lambda (instruction)
                     (remove-if-not
                      (lambda (output)
                        (typep output 'cleavir-ir:lexical-location))
                      (cleavir-ir:outputs instruction))))))
    (labels ((traverse (instruction)
               (unless (gethash instruction table)
                 (setf (gethash instruction table) t)
                 (setf conflicts
                       (append (conflicts-instruction instruction liveness))))))
      (traverse initial-instruction))
    (remove-duplicates conflicts :test #'same-conflict-p)))
