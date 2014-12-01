(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DATUM.  
;;;
;;; This is the root class of all different kinds of data. 

(defclass datum ()
  ((%defining-instructions :initform '() :accessor defining-instructions)
   (%using-instructions :initform '() :accessor using-instructions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions.

(defgeneric inputs (instruction))

(defgeneric (setf inputs) (new-inputs instruction))

(defmethod (setf inputs) :before (new-inputs instruction)
  (assert (listp new-inputs))
  (assert (every (lambda (input) (typep input 'datum)) new-inputs))
  ;; Remove this instruction as a using instruction from the existing
  ;; inputs.
  (loop for input in (inputs instruction)
	do (setf (using-instructions input)
		 (remove instruction (using-instructions input)
			 :test #'eq))))

(defmethod (setf inputs) :after (new-inputs instruction)
  ;; Add this instruction as a using instruction to the existing
  ;; inputs.
  (loop for input in (inputs instruction)
	do (push instruction (using-instructions input))))

(defgeneric outputs (instruction))

(defgeneric (setf outputs) (new-outputs instruction))

(defmethod (setf outputs) :before (new-outputs instruction)
  (assert (listp new-outputs))
  (assert (every (lambda (output) (typep output 'datum)) new-outputs))
  ;; Remove this instruction as a defining instruction from the
  ;; existing outputs.
  (loop for output in (outputs instruction)
	do (setf (defining-instructions output)
		 (remove instruction (defining-instructions output)
			 :test #'eq))))

(defmethod (setf outputs) :after (new-outputs instruction)
  ;; Add this instruction as a defining instruction to the existing
  ;; outputs.
  (loop for output in (outputs instruction)
	do (push instruction (defining-instructions output))))

(defclass instruction ()
  ((%predecessors :initform '() :initarg :predecessors :accessor predecessors)
   (%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :accessor inputs)
   (%outputs :initform '() :initarg :outputs :accessor outputs)))

(defmethod initialize-instance :after ((obj instruction) &key &allow-other-keys)
  (unless (and (listp (successors obj))
	       (every (lambda (successor)
			(typep successor 'instruction))
		      (successors obj)))
    (error "successors must be a list of instructions"))
  (assert (and (listp (inputs obj))
	       (every (lambda (input) (typep input 'datum)) (inputs obj))))
  (assert (and (listp (outputs obj))
	       (every (lambda (output) (typep output 'datum)) (outputs obj))))
  ;; Add this instruction as a using instruction to its inputs.
  (loop for input in (inputs obj)
	do (push obj (using-instructions input)))
  ;; Add this instruction as an assigning instruction to its outputs.
  (loop for output in (outputs obj)
	do (push obj (defining-instructions output)))
  ;; Add this instruction as a successor of its predecessors.
  (loop for successor in (successors obj)
	do (push obj (predecessors successor))))

(defgeneric clone-instruction (instruction))

(defmethod clone-instruction ((instruction instruction))
  (make-instance (class-of instruction)
    :inputs (inputs instruction)
    :outputs (outputs instruction)
    :successors (successors instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifying the instruction graph.

;;; Insert a new instruction N BEFORE an existing instruction E.  N
;;; will have E as its sole successors, and E will have N as its sole
;;; predecessor.  For every existing predecessor P of E, P will become
;;; a predecessor of N and N will replace E as a successor of P.
(defun insert-instruction-before (new existing)
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

;;; Delete and instruction I.  I must have a single successor S.  S
;;; replaces I as the successor of every predecessor P of I.  The
;;; predecessors of I become the predecessors of S.
(defun delete-instruction (instruction)
  (assert (= (length (successors instruction)) 1))
  (setf (inputs instruction) '())
  (setf (outputs instruction) '())
  (let ((successor (car (successors instruction)))
	(predecessors (predecessors instruction)))
    (loop for predecessor in predecessors
	  do (setf (successors predecessor)
		   (substitute successor instruction (successors predecessor))))
    ;; Avoid having our successor mention some of our predecessors
    ;; multiple times in case some of our predecessors are already a
    ;; predecessors of our successor.
    (setf (predecessors successor)
	  (remove instruction (predecessors successor)
		  :test #'eq))
    (loop for predecessor in predecessors
	  do (pushnew predecessor (predecessors successor)
		      :test #'eq))))

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
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for datum in (inputs instruction)
		       do (setf (using-instructions datum) '())
			  (setf (defining-instructions datum) '()))
		 (loop for datum in (outputs instruction)
		       do (setf (using-instructions datum) '())
			  (setf (defining-instructions datum) '()))
		 (mapc #'traverse (successors instruction)))))
      (traverse initial-instruction)))
  ;; In the second pass, we add each instruction as a using
  ;; instruction of its inputs, and a defining instruction of its
  ;; outputs.
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for datum in (inputs instruction)
		       do (push instruction (using-instructions datum)))
		 (loop for datum in (outputs instruction)
		       do (push instruction (defining-instructions datum)))
		 (mapc #'traverse (successors instruction)))))
      (traverse initial-instruction))))
