(cl:in-package #:cleavir-hir-transformations)

;;;; Recall that an ENTER-INSTRUCTION is the successor of no other
;;;; instruction.
;;;;
;;;; The ENTER-INSTRUCTIONs of a program constitute a directed tree.
;;;; An ENTER-INSTRUCTION I closer to the root of the tree than some
;;;; other ENTER-INSTRUCTION J is said to be located FURTHER OUT than
;;;; J.
;;;;
;;;; We introduce the concept of OWNERSHIP.  This concept is defined
;;;; for lexical variables and for instructions.  The OWNER of an
;;;; instruction or a lexical variable is an ENTER-INSTRUCTION.
;;;;
;;;; Ownership for instructions is defined as follows:
;;;;
;;;;   * The owner of an ENTER-INSTRUCTION is itself.
;;;;
;;;;   * For all instruction types, EXCEPT the UNWIND-INSTRUCTION, the
;;;;     owner of the successors of an instruction I is the same as
;;;;     the owner of I.
;;;;
;;;;   * The owner of the successor of an UNWIND-INSTRUCTION I is the
;;;;     value returned by calling (INVOCATION I).
;;;;
;;;; The owner of a datum D is the outermost ENTER-INSTRUCTION of all
;;;; the owners of all the instructions using D.
;;;;
;;;; Each ENTER-INSTRUCTION A defines a PROCEDURE which is a the set
;;;; of all the instructions owned by A.  We extend the definition of
;;;; ownership so that a procedure P is the owner of some instruction
;;;; or datum X if an only if the unique ENTER-INSTRUCTION of P is the
;;;; owner of X.
;;;;
;;;; A procedure P is said to DEPEND on a different procedure Q if and
;;;; only if P or any of its descendants owns an instruction I (which
;;;; must then be an UNWIND-INSTRUCTION) with a successor owned by Q,
;;;; or if some instruction I owned by P or any of its descendants
;;;; refers to a datum owned by Q.
;;;;
;;;; The DROP of a procedure P is defined recursively as follows:
;;;;
;;;;   * If P depends on no other procedure then its drop is 0.
;;;;
;;;;   * Otherwise, the drop of P is d+1, where d is the
;;;;     maximum drop of any of the procedures it depends on.
;;;;
;;;; The drop is important for the following reason.  The runtime
;;;; environment could be organized as a list of LEVELS, where each
;;;; level is a vector.  A procedure P then has a runtime environment
;;;; with N = d+1 levels in it where d is the drop of P.  When some
;;;; procedure P with drop d executes an ENCLOSE-INSTRUCTION with some
;;;; procedure Q with drop e as an argument, then the top d-e+1 levels
;;;; of the static runtime environment of P should be discarded in
;;;; order to obtain the enclosed runtime environment.  Furthermore if
;;;; some procedure P with drop d accesses a variable owned by a
;;;; procedure Q with drop e, then level d-e of the static runtime
;;;; should be consulted.

(defun data (instruction)
  (append (cleavir-ir:inputs instruction)
	  (cleavir-ir:outputs instruction)))

(defvar *ownerships*)

(defun owner (item)
  (gethash item *ownerships*))

(defun (setf owner) (new-owner item)
  (setf (gethash item *ownerships*) new-owner))

(defun has-owner-p (item)
  (nth-value 1 (gethash item *ownerships*)))

(defparameter *ld1-call-count* 0)
(defparameter *ld1-item-count* 0)
(defparameter *ld1-time* 0)

;;; Compute the owner of each instruction and each datum.  The return
;;; value is an EQ hash table mapping an instruction or a datum to its
;;; owner.
(defun compute-ownerships (initial-instruction)
  (let ((time (get-internal-run-time))
	(worklist '())
	(current-owner (if (typep initial-instruction
				  'cleavir-ir:enter-instruction)
			   initial-instruction
			   nil))
	(*ownerships* (make-hash-table :test #'eq)))
    (labels
	((traverse (instruction)
	   (unless (has-owner-p instruction)
		   (setf (owner instruction) current-owner)
		   (loop for datum in (data instruction)
			 do (unless (has-owner-p datum)
			      (setf (owner datum) current-owner)))
		   (when (typep instruction 'cleavir-ir:enclose-instruction)
		     (let ((code (cleavir-ir:code instruction)))
		       (setf worklist (append worklist (list code)))))
		   (loop for succ in (cleavir-ir:successors instruction)
			 do (traverse succ))
		   (loop for pred in (cleavir-ir:predecessors instruction)
			 unless (typep pred 'cleavir-ir:unwind-instruction)
			   do (traverse pred)))))
      (traverse initial-instruction)
      (loop until (null worklist)
	    do (setf current-owner (pop worklist))
	       (traverse current-owner)))
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
		      (when (typep instruction 'cleavir-ir:enclose-instruction)
			(let ((code (cleavir-ir:code instruction)))
			  (setf worklist (append worklist (list code)))))
		      (loop with i-owner = (owner instruction)
			    for datum in (data instruction)
			    for d-owner = (owner datum)
			    do (unless (eq d-owner i-owner)
				 (setf (lexical-depth enter-instruction)
				       (max (lexical-depth enter-instruction)
					    (1+ (lexical-depth d-owner))))))
		      (loop with i-owner = (owner instruction)
			    for successor in (cleavir-ir:successors instruction)
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

;;; By SEGREGATING lexical locations, we mean taking each lexical
;;; location and turning it into either a dynamic lexical location
;;; (which can be allocated in a register or on the stack) or an
;;; static lexical location (which may or may not be possible to
;;; allocate on the stack, and might have to be allocated in some
;;; other place, possibly on the heap).
;;;
;;; The method used here is very simple, and not particularly
;;; sophisticated.  It assumes that every nested function can escape
;;; in arbitrary ways, so that every lexical location that is shared
;;; by some function F and some other function G nested inside F must
;;; be an static lexical location.
;;;
;;; We detect whether a lexical location is shared in this way by
;;; looking at the instructions that define it and use it.  If these
;;; instructions all have the same nesting depth, then the lexical
;;; location is not shared, otherwise it is.

(defun process-lexical (lexical nesting-depth)
  (let ((depths (loop with def = (cleavir-ir:defining-instructions lexical)
		      with use = (cleavir-ir:using-instructions lexical)
		      for instruction in (append def use)
		      collect (gethash instruction nesting-depth))))
    (change-class lexical
		  (if (> (length (remove-duplicates depths)) 1)
		      'cleavir-ir:static-lexical-location
		      'cleavir-ir:dynamic-lexical-location))))

(defun segregate-lexicals (initial-instruction)
  ;; Make sure everything is up to date.
  (cleavir-ir:reinitialize-data initial-instruction)
  (let ((nesting-depth (compute-nesting-depth initial-instruction))
	(table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop with inputs = (cleavir-ir:inputs instruction)
		       with outputs = (cleavir-ir:outputs instruction)
		       for datum in (append inputs outputs)
		       do (when (eq (class-of datum)
				    (find-class 'cleavir-ir:lexical-location))
			    (process-lexical datum nesting-depth))))))
      (traverse initial-instruction))))

(defun find-imports (initial-instruction)
  (let ((ownerships (compute-ownerships initial-instruction)))
    (flet ((process-datum (procedure-enter-instruction datum)
	     (when (and (typep datum 'cleavir-ir:static-lexical-location)
			(not (eq (gethash datum ownerships)
				 procedure-enter-instruction)))
	       (pushnew datum (cleavir-ir:imports procedure-enter-instruction)
			:test #'eq))))
      (traverse initial-instruction
		(lambda (instruction owner)
		  (loop for datum in (cleavir-ir:inputs instruction)
			do (process-datum owner datum))
		  (loop for datum in (cleavir-ir:outputs instruction)
			do (process-datum owner datum))
		  (when (typep instruction 'cleavir-ir:enclose-instruction)
		    (loop with code = (cleavir-ir:code instruction)
			  for import in (cleavir-ir:imports code)
			  do (process-datum owner import))))))))

;;; Create a "static map" for each ENTER-INSTRUCTION in a program.  A
;;; static map is a list of pairs (an association list) where the CAR
;;; of each CONS cell is a STATIC-LEXICAL-LOCATION and the CDR is a
;;; DYNAMIC-LEXICAL-LOCATION.  The meaning of a pair of the list is
;;; that the CELL that holds the value of the STATIC-LEXICAL-LOCATION
;;; is value of the DYNAMIC-LEXICAL-LOCATION.  The static lexical
;;; locations in the list are those that are used by the instructions
;;; owned by the corresponding ENTER-INSTRUCTION.  The dynamic lexical
;;; locations are freshly created by this function.
;;;
;;; The return value is an EQ hash table with the ENTER-INSTRUCTIONS
;;; as keys, and the associated hash value is the static map.
(defun create-static-map (enter-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (flet ((process-datum (owner datum)
	     (when (and (typep datum 'cleavir-ir:static-lexical-location)
			(not (member datum (gethash owner result)
				     :test #'eq :key #'car)))
	       (let* ((name (cleavir-ir:name datum))
		      (var (cleavir-ir:make-dynamic-lexical-location name)))
		 (push (cons datum var)
		       (gethash owner result))))))
      (traverse enter-instruction
		(lambda (instruction owner)
		   (loop for datum in (cleavir-ir:inputs instruction)
			 do (process-datum owner datum))
		   (loop for datum in (cleavir-ir:outputs instruction)
			 do (process-datum owner datum)))))))

(defun process-captured-variables (initial-instruction)
  (let ((owners (make-hash-table :test #'eq))
	(imports (make-hash-table :test #'eq))
	;; This table maps pairs of the form
	;; (<enter-instruction> . <static-lexical-location>)
	;; to dynamic lexical locations holding the cell for
	;; the captured variable.
	(cell-locations (make-hash-table :test #'equal)))
    (flet (;; This function adds a FETCH instruction before
	   ;; INSTRUCTION.  OWNER is the owner if INSTRUCTION, and it
	   ;; is known that OWNER is NOT the owner of LOCATION, so
	   ;; that LOCATION is one of the locations imported to OWNER.
	   (fetch-cell (instruction owner location)
	     (let ((pos (position location (gethash owner imports))))
	       (when (null pos)
		 ;; We haven't seen this location before.  Add it to
		 ;; the imports of OWNER.  We add it to the end so
		 ;; that the position of each of the ones we have
		 ;; already seen remains the same.
		 (setf (gethash owner imports)
		       (append (gethash owner imports) (list location)))
		 (setf pos (1- (length (gethash owner imports)))))
	       ;; We must now generate a FETCH instruction to load the
	       ;; cell from the static environment
	       (let* ((temp (cleavir-ir:new-temporary))
		      (env-location (first (cleavir-ir:outputs owner)))
		      (index (cleavir-ir:make-immediate-input pos))
		      (fetch (cleavir-ir:make-fetch-instruction
			      env-location index temp)))
		 (cleavir-ir:insert-instruction-before fetch instruction)
		 ;; Return the temporary holding the cell.
		 temp))))
      (traverse
       initial-instruction
       (lambda (instruction owner)
	 (setf (cleavir-ir:inputs instruction)
	       (loop for input in (cleavir-ir:inputs instruction)
		     collect
		     (if (typep input 'cleavir-ir:static-lexical-location)
			 (if (eq (gethash input owners) owner)
			     ;; The owner of this instruction is also
			     ;; the owner of the captured variable.
			     ;; This means that we already allocated a
			     ;; dynamic lexical location for it.
			     (let* ((location (gethash (cons owner input)
						       cell-locations))
				    (temp (cleavir-ir:new-temporary))
				    (inst (cleavir-ir:make-read-cell-instruction
					   location temp)))
			       ;; Insert the READ-CELL instruction
			       ;; before the current one.
			       (cleavir-ir:insert-instruction-before
				inst instruction)
			       ;; Return the new input to this instruction. 
			       temp)
			     ;; The owner of this instruction is not
			     ;; the owner of the captured variable.
			     ;; We need to fetch the cell from our
			     ;; static environment.
			     (let ((cell-location (fetch-cell instruction
							      owner
							      input)))
			     ;; We must now generate a READ-CELL
			     ;; instruction to read the value into a
			     ;; temporary location.
			     (let ((temp (cleavir-ir:new-temporary)))
			       (cleavir-ir:insert-instruction-before
				(cleavir-ir:make-read-cell-instruction
				 cell-location temp)
				instruction)
			       ;; Return the new input to this
			       ;; instruction.
			       temp)))
			 ;; This input is not a captured variable,
			 ;; return it unchanged
			 input)))
       ;; Now process the outputs of this instruction
       (setf (cleavir-ir:outputs instruction)
	     (loop for output in (cleavir-ir:outputs instruction)
		   collect
		   (if (typep output 'cleavir-ir:static-lexical-location)
		       (if (eq (gethash output owners) owner)
			   ;; The owner of this instruction is also
			   ;; the owner of the captured variable.
			   ;; Check whether we have written to it
			   ;; before.  If we have, then we have
			   ;; allocated a location for the
			   ;; corresponding cell.
			   (let ((location (gethash (cons owner output)
						    cell-locations)))
			     (when (null location)
			       ;; This write is the defining write to
			       ;; the variable, so we must allocate a
			       ;; location for its cell.
			       (setf location (cleavir-ir:new-temporary))
			       (setf (gethash (cons owner output)
					      cell-locations)
				     location))
			     ;; We must now change the current output
			     ;; to a temporary dynamic lexical
			     ;; location, and then add a WRITE-CELL
			     ;; instruction after this one to write
			     ;; the contents of the temporary to the
			     ;; cell.
			     (let ((temp (cleavir-ir:new-temporary))
				   (succs (cleavir-ir:successors instruction)))
			       (cleavir-ir:insert-instruction-after
				(cleavir-ir:make-write-cell-instruction
				 location temp (first succs))
				instruction)
			       ;; Finally return the temporary location.
			       temp))
			   ;; the owner if this instruction is not the
			   ;; owner of the captured variable.  We need
			   ;; to fetch the cell from our static
			   ;; environment.
			   (let ((cell-location (fetch-cell instruction
							    owner
							    output)))
			     ;; We must now change the current output
			     ;; to a temporary dynamic lexical
			     ;; location, and then add a WRITE-CELL
			     ;; instruction to write the contents of
			     ;; that temporary location to the cell.
			     (let ((temp (cleavir-ir:new-temporary)))
			       (cleavir-ir:insert-instruction-after
				(cleavir-ir:make-write-cell-instruction
				 cell-location temp)
				instruction)
			       ;; Return the new input to this
			       ;; instruction.
			       temp)))
		       ;; This input is not a captured variable,
		       ;; return it unchanged.
		       output))))))))
