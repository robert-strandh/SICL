(cl:in-package #:cleavir-lexical-depth)

;;;; Recall that an ENTER-INSTRUCTION is the successor of no other
;;;; instruction.
;;;;
;;;; The OWNER of an instruction A is the outermost ENTER-INSTRUCTION
;;;; of all the ENTER-INSTRUCTIONs from which A is reachable.  The
;;;; owner of a datum D is the outermost ENTER-INSTRUCTION of all the
;;;; owners of all the instructions using D.
;;;;
;;;; Each ENTER-INSTRUCTION A defines a PROCEDURE which is a the set
;;;; of all the instructions owned by A.  We extend the definition of
;;;; ownership so that a procedure P is the owner of some instruction
;;;; or datum X if an only if the unique ENTER-INSTRUCTION of P is the
;;;; owner of X. 
;;;;
;;;; A procedure P is a LEXICAL PARENT of a procedure Q if and only if
;;;; either some instruction in P is the direct successor of some
;;;; instruction in Q, or some instruction A in Q refers to a datum
;;;; owned by P. 
;;;;
;;;; The LEXICAL DEPTH of a procedure P is defined recursively as
;;;; follows:
;;;; 
;;;;   * If P has no lexical parent then its lexical depth is 0.
;;;;
;;;;   * Otherwise, the lexical depth of P is d+1, where d is the
;;;      maximum depth of any of its lexical parents. 

(defun data (instruction)
  (append (cleavir-mir:inputs instruction)
	  (cleavir-mir:outputs instruction)))

(defvar *ownerships*)

(defun owner (item)
  (gethash item *ownerships*))

(defun (setf owner) (new-owner item)
  (setf (gethash item *ownerships*) new-owner))

(defparameter *ld1-call-count* 0)
(defparameter *ld1-item-count* 0)
(defparameter *ld1-time* 0)

;;; Compute the owner of each instruction and each datum.  The return
;;; value is an EQ hash table mapping an instruction or a datum to its
;;; owner.
(defun compute-ownerships (enter-instruction)
  (let ((time (get-internal-run-time))
	(worklist (list enter-instruction))
	(*ownerships* (make-hash-table :test #'eq)))
    (flet
	((process-function (enter-instruction)
	   (labels
	       ((traverse (instruction)
		  (when  (null (owner instruction))
		    (setf (owner instruction) enter-instruction)
		    (loop for datum in (data instruction)
			  do (when (null (owner datum))
			       (setf (owner datum) enter-instruction)))
		    (when (typep instruction 'cleavir-mir:enclose-instruction)
		      (let ((code (cleavir-mir:code instruction)))
			(setf worklist (append worklist (list code)))))
		    (mapc #'traverse (cleavir-mir:successors instruction)))))
	     (traverse enter-instruction))))
      (loop until (null worklist)
	    do (process-function (pop worklist))))
    (incf *ld1-call-count*)
    (incf *ld1-item-count* (hash-table-count *ownerships*))
    (incf *ld1-time* (- (get-internal-run-time) time))
    *ownerships*))

(defvar *lexical-depths*)

(defun lexical-depth (enter-instruction)
  (gethash enter-instruction *lexical-depths*))

(defun (setf lexical-depth) (new-depth enter-instruction)
  (setf (gethash enter-instruction *lexical-depths*) new-depth))

(defparameter *ld2-call-count* 0)
(defparameter *ld2-item-count* 0)
(defparameter *ld2-time* 0)

;;; Return the lexical depth of each instruction and each datum of the
;;; program.  The return value is an EQ hash table mapping each item
;;; (instruction or datum) to its lexical depth.
(defun lexical-depths (enter-instruction)
  (let ((time (get-internal-run-time))
	(*ownerships* (compute-ownerships enter-instruction))
	(worklist (list enter-instruction))
	(*lexical-depths* (make-hash-table :test #'eq)))
    (flet
	((process-function (enter-instruction)
	   (setf (lexical-depth enter-instruction) 0)
	   (let ((visited (make-hash-table :test #'eq)))
	     (labels
		 ((traverse (instruction)
		    (unless (gethash instruction visited)
		      (setf (gethash instruction visited) t)
		      (when (typep instruction 'cleavir-mir:enclose-instruction)
			(let ((code (cleavir-mir:code instruction)))
			  (setf worklist (append worklist (list code)))))
		      (loop with i-owner = (owner instruction)
			    for datum in (data instruction)
			    for d-owner = (owner datum)
			    do (unless (eq d-owner i-owner)
				 (setf (lexical-depth enter-instruction)
				       (max (lexical-depth enter-instruction)
					    (1+ (lexical-depth d-owner))))))
		      (loop with i-owner = (owner instruction)
			    for successor in (cleavir-mir:successors instruction)
			    for s-owner = (owner successor)
			    do (if (eq i-owner s-owner)
				   (traverse successor)
				   (setf (lexical-depth enter-instruction)
					 (max (lexical-depth enter-instruction)
					      (1+ (lexical-depth s-owner)))))))))
	       (traverse enter-instruction)))))
      (loop until (null worklist)
	    do (process-function (pop worklist))))
    ;; Add remaining instructions and data to table.
    (maphash (lambda (item owner)
	       (when (null (gethash item *lexical-depths*))
		 (setf (gethash item *lexical-depths*)
		       (gethash owner *lexical-depths*))))
	     *ownerships*)
    (incf *ld2-call-count*)
    (incf *ld2-item-count* (hash-table-count *lexical-depths*))
    (incf *ld2-time* (- (get-internal-run-time) time))
    *lexical-depths*))

(defparameter *ld3-call-count* 0)
(defparameter *ld3-node-count* 0)
(defparameter *ld3-time* 0)

(defun distinguish-lexical-variables (enter-instruction lexical-depths)
  (let ((time (get-internal-run-time))
	(*lexical-depths* lexical-depths)
	(visited (make-hash-table :test #'eq)))
    ;; First find all lexical locations that should be turned
    ;; into indefinite lexical locations.
    (labels ((traverse (instruction)
	       (unless (gethash instruction visited)
		 (setf (gethash instruction visited) t)
		 (loop with type = 'cleavir-mir:lexical-location
		       for datum in (data instruction)
		       do (when (and (typep datum type)
				     (/= (lexical-depth datum)
					 (lexical-depth instruction)))
			    (change-class datum
					  'cleavir-mir:indefinite-lexical-location)))
		 (loop for succ in (cleavir-mir:successors instruction)
		       do (traverse succ))
		 (when (typep instruction 'cleavir-mir:enclose-instruction)
		   (traverse (cleavir-mir:code instruction))))))
      (traverse enter-instruction))
    ;; Next find all lexical locations that were not converted in the
    ;; first step, and convert them to simple lexical locations.  It
    ;; is enough to check the outputs of each instruction because each
    ;; lexical location must be the output of at least one
    ;; instruction.
    (clrhash visited)
    (labels ((traverse (instruction)
	       (unless (gethash instruction visited)
		 (setf (gethash instruction visited) t)
		 (loop with type = '(and cleavir-mir:lexical-location
				         (not cleavir-mir:indefinite-lexical-location))
		       for datum in (cleavir-mir:outputs instruction)
		       do (when (typep datum type)
			    (change-class datum
					  'cleavir-mir:dynamic-lexical-location)))
		 (loop for succ in (cleavir-mir:successors instruction)
		       do (traverse succ))
		 (when (typep instruction 'cleavir-mir:enclose-instruction)
		   (traverse (cleavir-mir:code instruction))))))
      (traverse enter-instruction))
    (incf *ld3-call-count*)
    (incf *ld3-node-count* (hash-table-count visited))
    (incf *ld3-time* (- (get-internal-run-time) time))))
