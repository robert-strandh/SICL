(cl:in-package #:sicl-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(defun check-name-clause-position (clauses)
  (let ((name-clause-position
          (position-if (lambda (clause) (typep clause 'name-clause)) clauses
                       :from-end t)))
    (when (and (not (null name-clause-position)) (plusp name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(defun check-order-variable-clause-main-clause (clauses)
  (let ((last-variable-clause-position
          (position-if (lambda (clause)
                         (typep clause 'variable-clause))
                       clauses
                       :from-end t))
        (first-main-clause-position
          (position-if (lambda (clause)
                         (typep clause 'main-clause))
                       clauses)))
    (when (and (not (null last-variable-clause-position))
               (not (null first-main-clause-position))
               (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

(defun verify-clause-order (clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(defun check-variable-uniqueness (clauses)
  (let* ((variables (reduce #'append (mapcar #'bound-variables clauses)
                            :from-end t))
         (unique-variables (remove-duplicates variables :test #'eq)))
    (unless (= (length variables)
               (length unique-variables))
      (loop for var in unique-variables
            do (when (> (count var variables :test #'eq) 1)
                 (error 'multiple-variable-occurrences
                        :bound-variable var))))))

;;; Check that for a given accumulation variable, there is only one
;;; category.  Recall that the accumlation categores are represented
;;; by the symbols LIST, COUNT/SUM, and MAX/MIN.
(defun check-accumulation-categories (clauses)
  (let* ((descriptors (reduce #'append
                              (mapcar #'accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq (first d1) (first d2))
                           (eq (second d1) (second d2)))))
         (unique (remove-duplicates descriptors :test equal-fun)))
    (loop for remaining on unique
          do (let ((entry (member (first (first remaining))
                                  (rest remaining)
                                  :test #'eq
                                  :key #'first)))
               (unless (null entry)
                 (error "the accumulation variable ~s is used both~@
                         for ~s accumulation and ~s accumulation."
                        (first (first remaining))
                        (second (first remaining))
                        (second (first entry))))))))

;;; Check that there is no overlap between the bound variables and the
;;; accumulation variables.
(defun check-no-variable-overlap (clauses)
  (let ((bound-variables
          (reduce #'append (mapcar #'bound-variables clauses)
                  :from-end t))
        (accumulation-variables
          (mapcar #'first
                  (reduce #'append
                          (mapcar #'accumulation-variables clauses)))))
    (let ((intersection
            (intersection bound-variables accumulation-variables
                          :test #'eq)))
      (unless (null intersection)
        (error "The variable ~s is used both as an iteration variable~@
                and as an accumulation variable."
               (car intersection))))))

;;; FIXME: Add more analyses.
(defun analyze-clauses (clauses)
  (verify-clause-order clauses)
  (check-variable-uniqueness clauses)
  (check-accumulation-categories clauses)
  (check-no-variable-overlap clauses))
