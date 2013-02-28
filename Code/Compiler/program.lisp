(in-package #:sicl-program)

;;; A LEXICAL LOCATION is an explicit (present in source code) or
;;; implicit (allocated by the compiler) lexical "place" used to store
;;; local variables and temporaries.
;;;
;;; An instruction I REFERS TO a lexical location L if and only if a
;;; lexical-location object that contains L is either one of the
;;; inputs or one of the outputs of I.
;;;
;;; A lexical location can be referred to by several different
;;; instructions that belong to procedures at different nesting
;;; depths.  Because of the way lexical locations are created, if a
;;; lexical location is referred to by two different instructions
;;; belonging to two different code objects, A and B, and neither A is
;;; nested inside B nor B is nested inside A, then the location is
;;; also referred to by some procedure C inside which both A and B are
;;; nested.
;;;
;;; A lexical location L is said to be PRESENT in a procedure P if and
;;; only if some instruction belonging to P refers to L.  A location L
;;; is said to BELONG to a procedure P if L is present in P, and L is
;;; not present in a procedure inside which P is nested.  Because of
;;; the restriction in the previous paragraph, every location belongs
;;; to some unique procedure.  The procedure P to which a location
;;; belongs is called the OWNER of the location.
;;;
;;; The LEXICAL DEPTH of a procedure is a quantity that is less than
;;; or equal to the NESTING depth of that procedure.  We define it
;;; recursively as follows: The lexical depth of a procedure A such
;;; that every location and instruction that is present in A also
;;; belongs to A is defined to be 0.  For a procedure A with a
;;; location or an instruction present in it, but that belongs to a
;;; different procedure B, let D be the greatest depth of any such
;;; procedure B.  Then the lexical depth of A is D+1. 
;;;
;;; A SIMPLE INSTRUCTION CHAIN is a sequence of instructions, all
;;; belonging to the same procedure, such that every instruction in
;;; the sequence except the first is the unique successor in the
;;; instruction graph of its predecessor in the sequence, and every
;;; instruction in the sequence except the last is the unique
;;; predecessor in the instruction graph of its successor in the
;;; sequence.
;;;
;;; A BASIC BLOCK is a MAXIMAL SIMPLE INSTRUCTION CHAIN.  It is
;;; maximal in that if any predecessor in the instruction graph of the
;;; first instruction in the chain were to be included in the
;;; sequence, then the sequence is no longer a simple instruction
;;; chain, and if any successor in the instruction graph of the last
;;; instruction in the chain were to be included in the sequence, then
;;; the sequence is no longer a simple instruction chain. 
;;;
;;; Every instruction belongs to exactly one basic block.  In the
;;; degenerate case, the basic block to which an instruction belongs
;;; contains only that single instruction.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A PROGRAM represents a top-level form together with derived
;;; knowledge of that form.  To begin with, an instance of a program
;;; contains only the initial instruction of the graph of instructions
;;; that represent nested procedures.

(defclass program ()
  ((%current-timestamp :initform 0 :accessor current-timestamp)
   (%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; This time stamp is set when the instruction graph is altered in
   ;; some way.
   (%instructions-timestemp 
    :initform 0 :accessor instructions-timestamp)
   ;; This table maps each instruction of the program to an
   ;; instruction-info instance.
   (%instruction-info
    :initform (make-hash-table :test #'eq) :accessor instruction-info)
   ;; This time stamp is set whenever the instruction-info table is
   ;; recomputed.
   (%instruction-info-timestamp
    :initform 0 :accessor instruction-info-timestamp)
   ;; This time stamp is set whenever the instruction predecessors are
   ;; recomputed.
   (%instruction-predecessors-timestamp
    :initform 0 :accessor instruction-predecessors-timestamp)
   ;; This time stamp is set whenever the instruction ownership is
   ;; recomputed.
   (%instruction-ownership-timestamp
    :initform 0 :accessor instruction-ownership-timestamp)
   ;; This time stamp is set whenever the lexical depths of procedures
   ;; are recomputed.
   (%lexical-depths-timestamp
    :initform 0 :accessor lexical-depths-timestamp)
   ;; This table maps each lexical location of the program to a
   ;; location-info instance.
   (%location-info
    :initform (make-hash-table :test #'eq) :accessor location-info)
   ;; This time stamp is set whenever the location-info table is
   ;; recomputed.
   (%location-info-timestamp
    :initform 0 :accessor location-info-timestamp)
   ;; This time stamp is set whenever the location ownership is
   ;; recomputed.
   (%location-ownership-timestamp
    :initform 0 :accessor location-ownership-timestamp)
   ;; This time stamp is set whenever the location indices are
   ;; recomputed.
   (%location-indices-timestamp
    :initform 0 :accessor location-indices-timestamp)
   ;; This table maps each external of the program to an
   ;; external-info instance.
   (%external-info
    :initform (make-hash-table :test #'eq) :accessor external-info)
   ;; This time stamp is set whenever the external-info table is
   ;; recomputed.
   (%external-info-timestamp
    :initform 0 :accessor external-info-timestamp)
   ;; All the procedures of this program.
   (%procedures :initform '() :accessor procedures)
   ;; This time stamp is set whenever the procedures table is
   ;; recomputed.
   (%procedures-timestamp
     :initform 0 :accessor procedures-timestamp)
   ;; This time stamp is set whenever the basic blocks are
   ;; recomputed.
   (%basic-blocks-timestamp
     :initform 0 :accessor basic-blocks-timestamp)))

;;; This variable holds the current program that is being processed.
;;; By doing it this way, we avoid having to pass it as an argument to
;;; every function, and we also avoid the need for back pointers in
;;; every class.
(defparameter *program* nil)

(defun map-instructions (function)
  (assert (not (null *program*)))
  (maphash (lambda (instruction info)
	     (declare (ignore info))
	     (funcall function instruction))
	   (instruction-info *program*)))

(defun map-locations (function)
  (assert (not (null *program*)))
  (maphash (lambda (location info)
	     (declare (ignore info))
	     (funcall function location))
	   (location-info *program*)))

(defun map-externals (function)
  (assert (not (null *program*)))
  (maphash (lambda (external info)
	     (declare (ignore info))
	     (funcall function external))
	   (external-info *program*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction info.
;;;
;;; We need a lot of information about an instruction.  Rather than
;;; storing that information in the instruction itself, we store it in
;;; an INSTRUCTION-INFO object.  There is one such object for each
;;; instruction, we put it in a hash table with the instruction as a key. 

(defgeneric predecessors (instruction-or-info))
(defgeneric (setf predecessors) (new instruction-or-info))
(defgeneric owner (instruction-or-info))
(defgeneric (setf owner) (new instruction-or-info))
(defgeneric basic-block (instruction-or-info))
(defgeneric (setf basic-block) (new instruction-or-info))

(defclass instruction-info ()
  (;; The list of predecessor instructions of this instruction. 
   (%predecessors :initform '() :accessor predecessors)
   ;; The procedure to which this instruction belongs.
   (%owner :initform nil :accessor owner)
   ;; The basic block to which this instruction belongs.
   (%basic-block :initform nil :accessor basic-block)))

(defmethod predecessors ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (predecessors (gethash instruction (instruction-info *program*))))

(defmethod (setf predecessors) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (predecessors (gethash instruction (instruction-info *program*)))
	new))

(defmethod owner ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (owner (gethash instruction (instruction-info *program*))))

(defmethod (setf owner) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (owner (gethash instruction (instruction-info *program*)))
	new))

(defmethod basic-block ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (basic-block (gethash instruction (instruction-info *program*))))

(defmethod (setf basic-block) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (basic-block (gethash instruction (instruction-info *program*)))
	new))

(defun successors (instruction)
  (sicl-mir:successors instruction))

(defun inputs (instruction)
  (sicl-mir:inputs instruction))

(defun outputs (instruction)
  (sicl-mir:outputs instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating an INSTRUCTION-INFO instance for each instruction in the
;;; program.

(defun compute-instruction-info (program)
  (with-accessors ((instruction-info instruction-info))
      program
    (let ((*program* program))
      (clrhash instruction-info)
      (labels ((aux (instruction)
		 (when (null (gethash instruction instruction-info))
		   (setf (gethash instruction instruction-info)
			 (make-instance 'instruction-info))
		   (when (typep instruction 'sicl-mir:enclose-instruction)
		     (aux (sicl-mir:code instruction)))
		   (mapc #'aux (successors instruction)))))
	(aux (initial-instruction program))))))

(defun ensure-instruction-info (program)
  (when (>= (instructions-timestamp program)
	    (instruction-info-timestamp program))
    (compute-instruction-info program)
    (setf (instruction-info-timestamp program)
	  (incf (current-timestamp program))))
  (instruction-info-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location info.
;;;
;;; As with instructions, We need a som information about a
;;; location.  Again, rather than storing that information in the
;;; location itself, we store it in an LOCATION-INFO object.  There is
;;; one such object for each location, we put it in a hash table
;;; with the location as a key.

(defclass location-info ()
  (;; The procedure to which this location belongs.
   (%owner :initform nil :accessor owner)
   (%index :initform nil :accessor index)))

(defmethod owner ((location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (owner (gethash location (location-info *program*))))

(defmethod (setf owner) (new (location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (setf (owner (gethash location (location-info *program*)))
	new))

(defmethod index ((location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (index (gethash location (location-info *program*))))

(defmethod (setf index) (new (location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (setf (index (gethash location (location-info *program*)))
	new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; External info.
;;;
;;; As with lexical locations, We need a som information about a the
;;; externals of a program.  Again, rather than storing that
;;; information in the external itself, we store it in an
;;; EXTERNAL-INFO object.  There is one such object for each external,
;;; we put it in a hash table with the external as a key.

(defclass external-info ()
  ((%index :initform nil :accessor index)))

(defmethod index ((external sicl-mir:external-input))
  (assert (not (null *program*)))
  (index (gethash external (external-info *program*))))

(defmethod (setf index) (new (external sicl-mir:external-input))
  (assert (not (null *program*)))
  (setf (index (gethash external (external-info *program*)))
	new))

(defun compute-externals-info (program)
  (let ((*program* program)
	(externals '()))
    (with-accessors ((external-info external-info))
	program
      (clrhash external-info)
      (map-instructions
       (lambda (instruction)
	 (loop for input in (inputs instruction)
	       do (when (typep input 'sicl-mir:external-input)
		    (setf (gethash input external-info)
			  (make-instance 'external-info))
		    (pushnew (sicl-mir:value input) externals
			     :test #'equal)))))
      (map-externals
       (lambda (external)
	 (setf (index external)
	       (position (sicl-mir:value external) externals
			 :test #'equal)))))))
    
(defun ensure-externals-info (program)
  (when (>= (ensure-instruction-info program)
	    (external-info-timestamp program))
    (compute-externals-info program)
    (setf (external-info-timestamp program)
	  (incf (current-timestamp program))))
  (external-info-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An instances of the PROCEDURE class represents a single procedure.
;;; An instruction is the INITIAL INSTRUCTION of a procedure if and
;;; only if it is either the initial instruction of a PROGRAM (see
;;; below) or the INPUT of an ENCLOSE instruction.  The initial
;;; instruction of a procedure does not have any predecessors.  A
;;; procedure can be the input of at most one ENCLOSE instructions.
;;; To preserve this property, inlining a procedure must duplicate
;;; it, all the instructions in it, and all the lexical locations in
;;; it.
;;;
;;; The procedures of a program are nested, and the outermost
;;; procedure is at NESTING DEPTH 0.  A procedure B is IMMEDIATELY
;;; NESTED inside a procedure A if and only if its initial instruction
;;; is the INPUT to an ENCLOSE instruction that belongs to procedure
;;; A.  A procedure B immediately nested inside a procedure A has a
;;; nesting depth with is one greater than the nesting depth of A.
;;;
;;; An INSTRUCTION is said to be PRESENT in a procedure P if it can
;;; be reached from the initial instruction of A by following
;;; successor arcs only.
;;; 
;;; An INSTRUCTION is said to BELONG to a procedure P if it is present
;;; in P, but it is not present in any procedure with a smaller
;;; nesting depth than that of P.  The procedure P to which an
;;; instruction I belongs is called the OWNER of I.
;;; 
;;; An INSTRUCTION can be present in several procedures, but can
;;; belong to only one.  An instruction is present in more than one
;;; procedures when it belongs to some procedure P, but it can also be
;;; reached from an instruction in a procedure Q nested inside P, so
;;; that Q has a nesting depth that is greater than that of P. Such a
;;; situation is the result of a RETURN-FROM or a GO form that
;;; transfers control from one procedure to an enclosing procedure.

(defclass procedure ()
  ((%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; Instructions that belong to this procedure. 
   (%instructions :initform '() :accessor instructions)
   ;; Locations that  belong to this procedure. 
   (%locations :initform '() :accessor locations)
   (%nesting-depth :initarg :nesting-depth :accessor nesting-depth)
   (%lexical-depth :initform nil :accessor lexical-depth)
   ;; The basic blocks that belong to this procedure.
   (%basic-blocks :initform '() :accessor basic-blocks)))

(defun make-procedure (initial-instruction nesting-depth)
  (make-instance 'procedure
    :initial-instruction initial-instruction
    :nesting-depth nesting-depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For each instruction in a complete instruction graph, determine
;;; all the nested procedures, and to which procedure each
;;; instruction belongs.

(defun compute-instruction-ownership (program)
  (with-accessors ((initial-instruction initial-instruction)
		   (instruction-info instruction-info)
		   (procedures procedures))
      program
    (let ((*program* program))
      (let* ((first (make-procedure initial-instruction 0))
	     (worklist (list first)))
	(push first procedures)
	(flet
	    ((handle-single-procedure (procedure)
	       (labels
		   ((aux (instruction)
		      (when (null (owner instruction))
			(setf (owner instruction) procedure)
			(when (typep instruction 'sicl-mir:enclose-instruction)
			  (let ((new (make-procedure
				      (sicl-mir:code instruction)
				      (1+ (nesting-depth procedure)))))
			    (push new procedures)
			    (setf worklist (append worklist (list new)))))
			(mapc #'aux (successors instruction)))))
		 (aux (initial-instruction procedure)))))
	  (loop until (null worklist)
		do (handle-single-procedure (pop worklist)))))
      (map-instructions
       (lambda (instruction)
	 (push instruction (instructions (owner instruction))))))))

(defun ensure-instruction-ownership (program)
  (when (>= (ensure-instruction-info program)
	    (instruction-ownership-timestamp program))
    (compute-instruction-ownership program)
    (setf (instruction-ownership-timestamp program)
	  (incf (current-timestamp program))))
  (instruction-info-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the predecessors of every instruction in a program.

(defun compute-predecessors (program)
  (let ((*program* program))
    (map-instructions
     (lambda (instruction)
       (setf (predecessors instruction) '())))
    (map-instructions
     (lambda (instruction)
       (loop for successor in (successors instruction)
	     do (push instruction (predecessors successor)))))))

(defun ensure-predecessors (program)
  (when (>= (ensure-instruction-info program)
	    (instruction-predecessors-timestamp program))
    (compute-predecessors program)
    (setf (instruction-predecessors-timestamp program)
	  (incf (current-timestamp program))))
  (instruction-predecessors-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute lexical location info.
;;;
;;; For each lexical location in the program, create a location-info
;;; instance and put it in the location-info hash table.

(defun compute-location-info (program)
  (let ((*program* program))
    (with-accessors ((location-info location-info))
	program
      (clrhash location-info)
      (map-instructions
       (lambda (instruction)
	 (loop for location in (append (inputs instruction)
				       (outputs instruction))
	       do (when (and (typep location 'sicl-env:lexical-location)
			     (null (gethash location location-info)))
		    (setf (gethash location location-info)
			  (make-instance 'location-info)))))))))
			
(defun ensure-location-info (program)
  (when (>= (ensure-instruction-info program)
	    (location-info-timestamp program))
    (compute-location-info program)
    (setf (location-info-timestamp program)
	  (incf (current-timestamp program))))
  (location-info-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute location ownership.

(defun compute-location-ownership-for-procedure (procedure)
  (loop for instruction in (instructions procedure)
	do (loop for location in (append (inputs instruction)
					 (outputs instruction))
		 do (when (and (typep location 'sicl-env:lexical-location)
			       (null (owner location)))
		      (setf (owner location) procedure)))))

(defun compute-location-ownership (program)
  (let ((*program* program))
    (with-accessors ((procedures procedures))
	program
      (let ((sorted-code
	      (sort (copy-list procedures) #'< :key #'nesting-depth)))
	(loop for procedure in sorted-code
	      do (compute-location-ownership-for-procedure procedure)))
      (map-locations (lambda (location)
		       (push location (locations (owner location))))))))

(defun ensure-location-ownership (program)
  (when (>= (max (instruction-ownership-timestamp program)
		 (location-info-timestamp program))
	    (location-ownership-timestamp program))
    (compute-location-ownership program)
    (setf (location-ownership-timestamp program)
	  (incf (current-timestamp program))))
  (location-ownership-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the LEXICAl DEPTH of each procedure in a program.

(defun compute-lexical-depth (procedure)
  (setf (lexical-depth procedure)
	(loop for location in (locations procedure)
	      maximize  (let ((owner (owner location)))
			  (if (eq owner procedure)
			      0
			      (1+ (lexical-depth owner)))))))

(defun compute-lexical-depths (program)
  (let ((*program* program))
    (with-accessors ((procedures procedures))
	program
      (let ((sorted-code
	      (sort (copy-list procedures) #'< :key #'nesting-depth)))
	(loop for procedure in sorted-code
	      do (compute-lexical-depth procedure))))))

(defun ensure-lexical-depths (program)
  (when (>= (ensure-location-ownership program)
	    (lexical-depths-timestamp program))
    (compute-lexical-depths program)
    (setf (lexical-depths-timestamp program)
	  (incf (current-timestamp program))))
  (lexical-depths-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the index of each lexical location.

(defun compute-location-indices (program)
  (let ((*program* program)
	(indices (make-hash-table :test #'eq)))
    (with-accessors ((procedures procedures))
	program
      (loop for procedure in procedures
	    do (setf (gethash procedure indices) 0))
      (map-locations (lambda (location)
		       (setf (index location)
			     (gethash (procedure location) indices))
		       (incf (gethash (procedure location) indices)))))))

(defun ensure-location-indices (program)
  (when (>= (ensure-instruction-ownership program)
	    (location-indices-timestamp program))
    (compute-location-indices program)
    (setf (location-indices-timestamp program)
	  (incf (current-timestamp program))))
  (location-indices-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic blocks.

(defclass basic-block ()
  ((%initial :initarg :initial :accessor initial)
   (%final :initarg :final :accessor final)))

(defun compute-basic-blocks-for-procedure (procedure program)
  (ensure-instruction-ownership program)
  (with-accessors ((instructions instructions)
		   (basic-blocks basic-blocks))
      procedure
    (let ((remaining instructions))
      (flet ((one-block (instruction)
	       (let ((initial instruction)
		     (final instruction))
		 (loop for preds = (predecessors initial)
		       while (= (length preds) 1)
		       for pred = (car preds)
		       while (eq (procedure pred) procedure)
		       for succs = (successors pred)
		       while (= (length succs) 1)
		       do (setf initial pred)
			  (setf remaining (remove pred remaining :test #'eq)))
		 (loop for succs = (successors final)
		       while (= (length succs) 1)
		       for succ = (car succs)
		       while (eq (procedure succ) procedure)
		       for preds = (predecessors succ)
		       while (= (length preds) 1)
		       do (setf final succ)
			  (setf remaining (remove succ remaining :test #'eq)))
		 (make-instance 'basic-block
		   :initial initial
		   :final final))))
	(loop until (null remaining)
	      do (push (one-block (pop remaining)) basic-blocks))))))

(defun compute-basic-blocks (program)
  (loop for procedure in (procedures program)
	do (compute-basic-blocks-for-procedure procedure program)))
  
(defun ensure-basic-blocks (program)
  (when (>= (ensure-instruction-ownership program)
	    (basic-blocks-timestamp program))
    (compute-basic-blocks program)
    (setf (basic-blocks-timestamp program)
	  (incf (current-timestamp program))))
  (basic-blocks-timestamp program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun post-process (initial-instruction)
  (let* ((program (make-instance 'program
		    :initial-instruction initial-instruction))
	 (*program* program))
    (compute-instruction-ownership program)
    (compute-lexical-depths program)
    (compute-location-ownership program)
    (compute-location-indices program)
    (compute-externals-info program)
    (compute-basic-blocks program)
    program))
