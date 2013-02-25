(in-package #:sicl-optimize)

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
;;; A lexical location L is said to be PRESENT in a procedure A if and
;;; only if some instruction belonging to A refers to L.  A location L
;;; is said to BELONG to a procedure A if L is present in A, and L is
;;; not present in a procedure inside which A is nested.  Because of
;;; the restriction in the previous paragraph, every location belongs
;;; to some unique procedure.
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
  ((%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; This table maps each instruction to a list of predecessor
   ;; instructions. 
   (%predecessors :initform nil :accessor predecessors)
   ;; This table maps each instruction of the program to
   ;; an instruction-info instance. 
   (%instruction-info :initform nil :accessor instruction-info)
   ;; This table maps each lexical location of the program to a
   ;; location-info instance.
   (%location-info :initform nil :accessor location-info)
   ;; All the procedures of this program.
   (%procedures :initform '() :accessor procedures)))

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
(defgeneric procedure (instruction-or-info))
(defgeneric (setf procedure) (new instruction-or-info))
(defgeneric basic-block (instruction-or-info))
(defgeneric (setf basic-block) (new instruction-or-info))
(defgeneric pre-locations (instruction-or-info))
(defgeneric (setf pre-locations) (new instruction-or-info))
(defgeneric post-locations (instruction-or-info))
(defgeneric (setf post-locations) (new instruction-or-info))

(defclass instruction-info ()
  (;; The list of predecessor instructions of this instruction. 
   (%predecessors :initform nil :accessor predecessors)
   ;; The procedure to which this instruction belongs.
   (%procedure :initform nil :accessor procedure)
   ;; The basic block to which this instruction belongs.
   (%basic-block :initform nil :accessor basic-block)
   ;; A set of CONS pairs (location . distance) that are valid
   ;; immediatly before this instruction.
   (%pre-locations :initform nil :accessor pre-locations)
   ;; A set of CONS pairs (location . distance) that are valid
   ;; immediatly after this instruction.
   (%post-locations :initform nil :accessor post-locations)))

(defmethod predecessors ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (predecessors (gethash instruction (instruction-info *program*))))

(defmethod (setf predecessors) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (predecessors (gethash instruction (instruction-info *program*)))
	new))

(defmethod procedure ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (procedure (gethash instruction (instruction-info *program*))))

(defmethod (setf procedure) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (procedure (gethash instruction (instruction-info *program*)))
	new))

(defmethod basic-block ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (basic-block (gethash instruction (instruction-info *program*))))

(defmethod (setf basic-block) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (basic-block (gethash instruction (instruction-info *program*)))
	new))

(defmethod pre-locations ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (pre-locations (gethash instruction (instruction-info *program*))))

(defmethod (setf pre-locations) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (pre-locations (gethash instruction (instruction-info *program*)))
	new))

(defmethod post-locations ((instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (post-locations (gethash instruction (instruction-info *program*))))

(defmethod (setf post-locations) (new (instruction sicl-mir:instruction))
  (assert (not (null *program*)))
  (setf (post-locations (gethash instruction (instruction-info *program*)))
	new))

(defun successors (instruction)
  (sicl-mir:successors instruction))

(defun inputs (instruction)
  (sicl-mir:inputs instruction))

(defun outputs (instruction)
  (sicl-mir:outputs instruction))

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
  (;; The procedure to which this instruction belongs.
   (%procedure :initform nil :accessor procedure)))

(defmethod procedure ((location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (procedure (gethash location (location-info *program*))))

(defmethod (setf procedure) (new (location sicl-env:lexical-location))
  (assert (not (null *program*)))
  (setf (procedure (gethash location (location-info *program*)))
	new))

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
;;; An INSTRUCTION is said to be PRESENT in a procedure A if it can
;;; be reached from the initial instruction of A by following
;;; successor arcs only.
;;; 
;;; An INSTRUCTION is said to BELONG to a procedure A if it is
;;; present in A, but it is not present in any procedure with a
;;; smaller nesting depth than that of A. 
;;; 
;;; An INSTRUCTION can be present in several procedures, but can
;;; belong to only one.  An instruction is present in more than one
;;; procedures when it belongs to some procedure A, but it can also be
;;; reached from an instruction in a procedure B nested inside A, so
;;; that B has a nesting depth that is greater than that of A. Such a
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
      (setf instruction-info (make-hash-table :test #'eq))
      (let* ((first (make-procedure initial-instruction 0))
	     (worklist (list first)))
	(push first procedures)
	(flet
	    ((handle-single-procedure (procedure)
	       (labels
		   ((aux (instruction)
		      (when (null (gethash instruction instruction-info))
			(setf (gethash instruction instruction-info)
			      (make-instance 'instruction-info))
			(setf (procedure instruction) procedure)
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
	 (push instruction (instructions (procedure instruction))))))))

(defun ensure-instruction-ownership (program)
  (when (null (instruction-info program))
    (compute-instruction-ownership program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the predecessors of every instruction in a program.

(defun compute-predecessors (program)
  (ensure-instruction-ownership program)
  (let ((*program* program))
    (map-instructions
     (lambda (instruction)
       (loop for successor in (successors instruction)
	     do (push instruction (predecessors successor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For each location in a complete instruction graph, determine
;;; to which procedure each location belongs.

(defun compute-location-ownership (program)
  (let ((*program* program))
    (with-accessors ((procedures procedures))
	program
      (ensure-instruction-ownership program)
      (setf (location-info program) (make-hash-table :test #'eq))
      (let ((sorted-code
	      (sort (copy-list procedures) #'< :key #'nesting-depth)))
	(loop for procedure in sorted-code
	      do (loop for instruction in (instructions procedure)
		       do (loop for info in (append (inputs instruction)
						    (outputs instruction))
				for location = (sicl-env:location info)
				do (when (null (procedure location))
				     (setf (procedure location) procedure))))))
      (map-locations (lambda (location)
		       (push location (locations (procedure location))))))))

(defun ensure-location-ownership (program)
  (when (null (location-info program))
    (compute-location-ownership program)))

(defun location-owner (location program)
  (ensure-location-ownership program)
  (gethash location (location-info program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the LEXICAl DEPTH of each procedure in a program.

(defun compute-lexical-depth (procedure program)
  (setf (lexical-depth procedure)
	(loop for location in (locations procedure)
	      maximize  (let ((owner (location-owner location program)))
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
	      do (compute-lexical-depth procedure program))))))

(defun ensure-lexical-depths (program)
  (when (member nil (procedures program) :key #'lexical-depth)
    (compute-lexical-depths program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine pre-locations and post-locations for each instruction.
;;;
;;; In this context, a FUTURE SET is a list of CONS cells.  The CAR
;;; of each CONS cell is a location, and the CDR is a non-negative
;;; integer indicating the distance (in number of instructions) to the
;;; next use of the location.  Each location can be present at most
;;; once in the list.  

(defun future-sets-equal-p (set1 set2)
  (and (= (length set1) (length set2))
       (null (set-difference set1 set2 :test #'equal))
       (null (set-difference set2 set1 :test #'equal))))

(defun combine-future-sets (set1 set2)
  (let ((result '()))
    (loop for element in (append set1 set2)
	  do (let ((existing (find (car element) result :key #'car :test #'eq)))
	       (if (null existing)
		   (push element result)
		   (setf (cdr existing)
			 (min (cdr existing) (cdr element))))))
    result))

(defun pre-from-post (instruction)
  (let ((in-locations (mapcar #'sicl-env:location (sicl-mir:inputs instruction)))
	(out-locations (mapcar #'sicl-env:location (sicl-mir:outputs instruction))))
    (let ((result (post-locations instruction)))
      ;; Remove the locations that are created by this instruction.
      (loop for out in out-locations
	    do (setf result (remove out result :key #'car)))
      ;; Increment the distance of the remaining ones.
      (loop for location in result
	    do (incf (cdr location)))
      ;; Combine with inputs.
      (setf (pre-locations instruction)
	    (combine-future-sets
	     result
	     (loop for in in in-locations
		   collect (cons in 0)))))))

(defun compute-pre-post-locations (program)
  (ensure-instruction-ownership program)
  (let ((*program* program))
    (let ((worklist '()))
      ;; Start by putting every instruction on the worklist.
      (map-instructions (lambda (instruction)
			  (push instruction worklist)))
      (loop until (null worklist)
	    for instruction = (pop worklist)
	    do (pre-from-post instruction)
	       (loop with pre = (pre-locations instruction)
		     for pred in (predecessors instruction)
		     for post = (post-locations pred)
		     for combined = (combine-future-sets pre post)
		     do (unless (future-sets-equal-p post combined)
			  (setf (post-locations pred)
				combined)
			  (unless (member pred worklist :test #'eq)
			    (if (= (length (successors pred)) 1)
				(push pred worklist)
				(setf worklist
				      (append worklist (list pred)))))))))))
			  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

(defclass assignment ()
  ((%free-regs :initarg :free-regs :reader free-regs)
   ;; A list of CONS cells (location . reg).
   (%in-regs :initarg :in-regs :reader in-regs)
   ;; A list of CONS cells (location . pos).
   (%on-stack :initarg :on-stack :reader on-stack)))

(defun make-assignment (free-regs in-regs on-stack)
  (make-instance 'assignment
    :free-regs free-regs
    :in-regs in-regs
    :on-stack on-stack))

;;; From a particular assignment and a future set, return two values:
;;; a new assignment which has at least one free register in it, and
;;; either a save instrution or nil if no saving is necessary.
(defun free-up-a-register (assignment future)
  (with-accessors ((free-regs free-regs)
		   (in-regs in-regs)
		   (on-stack on-stack))
      assignment
    (if (null free-regs)
	(let ((location-reg nil))
	  (loop with max = 0
		for loc in in-regs
		for time = (cdr (find (car loc) future :test #'eq :key #'car))
		do (when (> time max)
		     (setf max time)
		     (setf location-reg loc)))
	  ;; Check whether the location found is also on the stack
	  (if (member (car location-reg) on-stack :test #'eq :key #'car)
	      ;; If it is, then no saving is necessary.
	      (values 
	       (make-assignment (list (cdr location-reg))
				(remove location-reg in-regs :test #'eq)
				on-stack)
	       nil)
	      ;; If not, we need to save it.
	      (let ((pos (loop for pos from 0
			       unless (member pos on-stack :key #'cdr)
				 return pos)))
		(values
		 (make-assignment (list (cdr location-reg))
				  (remove location-reg in-regs :test #'eq)
				  (cons (cons (car location-reg) pos) on-stack))
		 `(save ,(cdr location-reg) ,pos)))))
	(values assignment nil))))
		 
;;; From a particular assignment, return two values: a new assignment
;;; in which all registers are free and a list of save instructions or
;;; nil if no saving is necessary.
(defun free-all-registers (assignment)
  (with-accessors ((free-regs free-regs)
		   (in-regs in-regs)
		   (on-stack on-stack))
      assignment
    (let ((instructions '())
	  (new-stack on-stack))
      (loop for loc in in-regs
	    do (unless (member (car loc) new-stack :test #'eq :key #'car)
		 (let ((pos (loop for pos from 0
				  unless (member pos new-stack :key #'cdr)
				    return pos)))
		   (push `(save ,(cdr loc) . ,pos) instructions)
		   (push `(,(car loc) . ,pos) new-stack))))
      (values
       (make-assignment (append (mapcar #'cdr in-regs) free-regs)
			'()
			new-stack)
       instructions))))
	  
;;; From a particular assignment and a future set, return a new
;;; assignment that is filtred so that only locations in the future
;;; set are on the stack or in registers.
(defun filter-assignment (assignment future)
  (with-accessors ((free-regs free-regs)
		   (in-regs in-regs)
		   (on-stack on-stack))
      assignment
    (let ((new-free-regs free-regs)
	  (new-in-regs '())
	  (new-stack '()))
      (loop for loc in in-regs
	    do (if (member (car loc) future :test #'eq :key #'car)
		   (push loc new-in-regs)
		   (push (cdr loc) new-free-regs)))
      (loop for loc in on-stack
	    do (when (member (car loc) future :test #'eq :key #'car)
		 (push loc new-stack)))
      (make-assignment new-free-regs
		       new-in-regs
		       new-stack))))

;;; From a paricular assignment and a location, return two values: a
;;; new assignment in which the location is in a register and a list
;;; of load and store instructions required to obtain the new
;;; assignment.
;; (defun ensure-location-in-register (assignment location)
;;   (with-accessors ((free-regs free-regs)
;; 		   (in-regs in-regs)
;; 		   (on-stack on-stack))
;;       assignment
;;     (cond ((member location in-regs :test #:eq :key #'car)
;; 	   (values assignment nil))
;; 	  ((

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

(defun ensure-basic-block-for-procedure (procedure program)
  (when (null (basic-blocks procedure))
    (compute-basic-blocks-for-procedure procedure program)))

(defun compute-basic-blocks (program)
  (ensure-instruction-ownership program)
  (loop for procedure in (procedures program)
	do (compute-basic-blocks-for-procedure procedure program)))
  
(defun ensure-basic-blocks (program)
  (ensure-instruction-ownership program)
  (loop for procedure in (procedures program)
	do (ensure-basic-block-for-procedure procedure program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point.

(defun post-process (initial-instruction)
  (let* ((program (make-instance 'program
		    :initial-instruction initial-instruction))
	 (*program* program))
    (compute-instruction-ownership program)
    (compute-basic-blocks program)
    program))
