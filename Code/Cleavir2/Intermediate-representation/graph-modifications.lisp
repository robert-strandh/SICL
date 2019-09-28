(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifying the instruction graph.

;;; Insert a new instruction N BEFORE an existing instruction E.  N
;;; will have E as its sole successors, and E will have N as its sole
;;; predecessor.  For every existing predecessor P of E, P will become
;;; a predecessor of N and N will replace E as a successor of P.
(defun insert-instruction-before (new existing)
  (reinitialize-instance new
    :dynamic-environment-location
    (dynamic-environment-location existing))
  (setf (predecessors new) (predecessors existing))
  (loop for pred in (predecessors existing)
	do (nsubstitute new existing (successors pred) :test #'eq))
  (setf (successors new) (list existing))
  (setf (predecessors existing) (list new)))

;;; Insert a new instruction N BETWEEN two exiting instruction E1 and
;;; E2, where E2 is a successor of E1.  E1 can have any number of
;;; successors and E2 can have any number of predecessors.  E1 becomes
;;; the sole predecessor of N, and E2 becomes the sole successor of N.
;;; N replaces E2 as a successor of E1, and E1 as a predecessor of E2.
(defun insert-instruction-between (new existing1 existing2)
  (reinitialize-instance new
    :dynamic-environment-location
    (dynamic-environment-location existing2))
  (setf (predecessors new) (list existing1))
  (setf (successors new) (list existing2))
  (nsubstitute new existing2 (successors existing1))
  (nsubstitute new existing1 (predecessors existing2)))

;;; Insert a new instruction N AFTER an existing instruction E.  E
;;; must have a single successor.  N is inserted BETWEEN E and its
;;; sole successor. 
(defun insert-instruction-after (new existing)
  (assert (= (length (successors existing)) 1))
  (insert-instruction-between new existing (car (successors existing))))

;;; Delete an instruction I.  I must have a single successor S.  S
;;; replaces I as the successor of every predecessor P of I.  The
;;; predecessors of I become the predecessors of S.
;;; If it so happens that I = S, the behavior is a bit different.
;;; For each predecessor P of I, P replaces I as a successor of P.
(defun delete-instruction (instruction)
  (assert (= (length (successors instruction)) 1))
  ;; Remove the instruction from datum records; this will spare us
  ;; a reinitialize-data.
  (loop for input in (inputs instruction)
        do (setf (using-instructions input)
                 (remove instruction (using-instructions input))))
  (setf (using-instructions (dynamic-environment-location instruction))
        (remove instruction (using-instructions
                             (dynamic-environment-location instruction))))
  (setf (inputs instruction) '())
  (loop for output in (outputs instruction)
        do (setf (defining-instructions output)
                 (remove instruction (defining-instructions output))))
  (setf (outputs instruction) '())
  ;; Delete the instruction from the control flow graph.
  (let ((successor (car (successors instruction)))
	(predecessors (predecessors instruction)))
    (cond ((eq successor instruction)
           ;; We have a loop.
           (loop for predecessor in predecessors
                 do (setf (successors predecessor)
                          (substitute predecessor instruction
                                      (successors predecessor)))
                    (pushnew predecessor (predecessors predecessor)
                             :test #'eq)))
          (t
           ;; Common case.
           (loop for predecessor in predecessors
                 do (setf (successors predecessor)
                          (substitute successor instruction (successors predecessor))))
           (cond ((and (typep instruction 'phi-instruction)
                       (rest predecessors)
                       (typep successor 'phi-instruction))
                  ;; When we delete the first phi in a phi cluster, we must
                  ;; take care to preserve the association between
                  ;; predecessors and inputs
                  (setf (predecessors successor)
                        (predecessors instruction)))
                 (t ;; Avoid having our successor mention some of our predecessors
                  ;; multiple times in case some of our predecessors are already a
                  ;; predecessors of our successor.
                  (setf (predecessors successor)
                        (remove instruction (predecessors successor)
                                :test #'eq))
                  (loop for predecessor in predecessors
                        do (pushnew predecessor (predecessors successor)
                                    :test #'eq))))))))

;;; Replace an instruction I with an instruction S, with respect to
;;; forward control flow.
;;; S replaces I as a successor of every predecessor of I.
;;; S gains all of I's predecessors as predecessors.
;;; This function orphans I and anything it dominates that is not
;;; dominated by S, so you'll probably have to set-predecessors etc.
(defun bypass-instruction (new existing)
  (setf (inputs existing) '()
	(outputs existing) '())
  (loop for predecessor in (predecessors existing)
	;; hook up the successors of P
	do (nsubstitute new existing (successors predecessor))
	;; and the predecessors of S
	do (pushnew predecessor (predecessors new))))

;;; When there has been some significant modifications to an
;;; instruction graph, it is possible that some instructions that are
;;; no longer reachable from the initial instruction refer to the same
;;; data as instructions that are still reachable.  In that case, we
;;; offer the possibility of reinitializing the data so that only
;;; reachable instructions are considered defining or using
;;; instructions.
(defun reinitialize-data (initial-instruction)
  ;; In the first pass, we set the defining and the using instructions
  ;; of every datum to the empty set.
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (inputs instruction)
	   do (setf (using-instructions datum) '())
	      (setf (defining-instructions datum) '()))
     (loop for datum in (outputs instruction)
	   do (setf (using-instructions datum) '())
	      (setf (defining-instructions datum) '())))
   initial-instruction)
  ;; In the second pass, we add each instruction as a using
  ;; instruction of its inputs, and a defining instruction of its
  ;; outputs.
  (map-instructions-arbitrary-order
   (lambda (instruction)
     (loop for datum in (inputs instruction)
	   do (push instruction (using-instructions datum)))
     (push instruction
           (using-instructions (dynamic-environment-location instruction)))
     (loop for datum in (outputs instruction)
	   do (push instruction (defining-instructions datum))))
   initial-instruction))
