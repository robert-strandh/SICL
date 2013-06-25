(in-package #:sicl-program)

;;;; A SIMPLE INSTRUCTION CHAIN is a sequence of instructions, all
;;;; belonging to the same procedure, such that every instruction in
;;;; the sequence except the first is the unique successor in the
;;;; instruction graph of its predecessor in the sequence, and every
;;;; instruction in the sequence except the last is the unique
;;;; predecessor in the instruction graph of its successor in the
;;;; sequence.
;;;;
;;;; A BASIC BLOCK is a MAXIMAL SIMPLE INSTRUCTION CHAIN.  It is
;;;; maximal in that if any predecessor in the instruction graph of the
;;;; first instruction in the chain were to be included in the
;;;; sequence, then the sequence is no longer a simple instruction
;;;; chain, and if any successor in the instruction graph of the last
;;;; instruction in the chain were to be included in the sequence, then
;;;; the sequence is no longer a simple instruction chain. 
;;;;
;;;; Every instruction belongs to exactly one basic block.  In the
;;;; degenerate case, the basic block to which an instruction belongs
;;;; contains only that single instruction.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A PROGRAM represents a top-level form together with derived
;;; knowledge of that form.  To begin with, an instance of a program
;;; contains only the initial instruction of the graph of instructions
;;; that represent nested procedures.

(defclass program ()
  ((%timestamps
    :initform (make-hash-table :test #'eq) :reader timestamps)
   (%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; This table maps each instruction of the program to an
   ;; instruction-info instance.
   (%instruction-info
    :initform (make-hash-table :test #'eq) :accessor instruction-info)
   ;; This table maps each lexical location of the program to a
   ;; location-info instance.
   (%location-info
    :initform (make-hash-table :test #'eq) :accessor location-info)
   ;; This table maps each external of the program to an
   ;; external-info instance.
   (%external-info
    :initform (make-hash-table :test #'eq) :accessor external-info)
   ;; All the procedures of this program.
   (%procedures :initform '() :accessor procedures)))

(set-processor 'instruction-graph nil)

(defmethod initialize-instance :after ((instance program) &key &allow-other-keys)
  (touch instance 'instruction-graph))

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

(defun (setf successors) (new-successors instruction)
  (setf (sicl-mir:successors instruction) new-successors))

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

(set-processor 'instruction-info 'compute-instruction-info)

(add-dependencies 'instruction-info
		  '(instruction-graph))

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
   ;; A list of all instructions that have this location as an output.
   (%assigning-instructions :initform nil :accessor assigning-instructions)
   ;; A list of all instructions that have this location as an input.
   (%using-instructions :initform nil :accessor using-instructions)
   (%index :initform nil :accessor index)))

(defmethod owner ((location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (owner (gethash location (location-info *program*))))

(defmethod (setf owner) (new (location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (setf (owner (gethash location (location-info *program*)))
	new))

(defmethod assigning-instructions ((location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (assigning-instructions (gethash location (location-info *program*))))

(defmethod (setf assigning-instructions)
    (new (location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (setf (assigning-instructions (gethash location (location-info *program*)))
	new))

(defmethod using-instructions ((location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (using-instructions (gethash location (location-info *program*))))

(defmethod (setf using-instructions) (new (location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (setf (using-instructions (gethash location (location-info *program*)))
	new))

(defmethod index ((location sicl-mir:lexical-location))
  (assert (not (null *program*)))
  (index (gethash location (location-info *program*))))

(defmethod (setf index) (new (location sicl-mir:lexical-location))
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
    
(set-processor 'externals-info 'compute-externals-info)

(add-dependencies 'externals-info
		  '(instruction-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROCEDURE.

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

(set-processor 'instruction-ownership 'compute-instruction-ownership)

(add-dependencies 'instruction-ownership
		  '(instruction-info))

;;; It might be confusing that we compute the procedures at the same
;;; time we determine instruction ownership.  For that reason, we
;;; instroduce a target named PROCEDURES.

(set-processor 'procedures 'identity)

(add-dependencies 'procedures
		  '(instruction-ownership))

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

(set-processor 'predecessors 'compute-predecessors)
	       
(add-dependencies 'predecessors
		  '(instruction-info))

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
	       do (when (and (typep location 'sicl-mir:lexical-location)
			     (null (gethash location location-info)))
		    (setf (gethash location location-info)
			  (make-instance 'location-info)))))))))

(set-processor 'location-info 'compute-location-info )

(add-dependencies 'location-info
		  '(instruction-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute location ownership.

(defun compute-location-ownership-for-procedure (procedure)
  (loop for instruction in (instructions procedure)
	do (loop for location in (append (inputs instruction)
					 (outputs instruction))
		 do (when (and (typep location 'sicl-mir:lexical-location)
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

(set-processor 'location-ownership 'compute-location-ownership)

(add-dependencies 'location-ownership
		  '(instruction-ownership
		    location-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the assigning and using instructions of each location.

(defun compute-location-assign-use (program)
  (let ((*program* program))
    ;; Start by clearing the assigning and using instructions
    ;; of each location.
    (loop for procedure in (procedures program)
	  do (loop for location in (locations procedure)
		   do (when (typep location 'sicl-mir:lexical-location)
			(setf (assigning-instructions location) nil)
			(setf (using-instructions location) nil))))
    ;; Next, loop over all instructions and for each instruction, add
    ;; it to the list of assigning instructions for all its outputs,
    ;; and addit to the list of using instructions for all its inputs.
    (loop for procedure in (procedures program)
	  do (loop for instruction in (instructions procedure)
		   do (loop for input in (inputs instruction)
			    do (when (typep input 'sicl-mir:lexical-location)
				 (push instruction
				       (using-instructions input))))
		      (loop for output in (outputs instruction)
			    do (when (typep output 'sicl-mir:lexical-location)
				 (push instruction
				       (assigning-instructions output))))))))

(set-processor 'location-assign-use 'compute-location-assign-use)

(add-dependencies 'location-assign-use
		  '(location-info
		    instruction-ownership))

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

(set-processor 'lexical-depth 'compute-lexical-depth)

(add-dependencies 'lexical-depth
		  '(location-ownership))

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
			     (gethash (owner location) indices))
		       (incf (gethash (owner location) indices)))))))

(set-processor 'location-indices 'compute-location-indices)

(add-dependencies 'location-indices
		  '(location-ownership))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic blocks.

(defclass basic-block ()
  ((%initial :initarg :initial :accessor initial)
   (%final :initarg :final :accessor final)))

(defun compute-basic-blocks-for-procedure (procedure)
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
		       while (eq (owner pred) procedure)
		       for succs = (successors pred)
		       while (= (length succs) 1)
		       do (setf initial pred)
			  (setf remaining (remove pred remaining :test #'eq)))
		 (loop for succs = (successors final)
		       while (= (length succs) 1)
		       for succ = (car succs)
		       while (eq (owner succ) procedure)
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
  (let ((*program* program))
    (loop for procedure in (procedures program)
	  do (compute-basic-blocks-for-procedure procedure))))

(set-processor 'basic-blocks 'compute-basic-blocks)

(add-dependencies 'basic-blocks
		  '(instruction-ownership))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find unused lexical locations of a program.

(defun find-unused-lexical-locations (program)
  (let ((*program* program)
	(result '()))
    (map-locations
     (lambda (location)
       (when (and (typep location 'sicl-mir:lexical-location)
		  (null (using-instructions location)))
	 (push location result))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Simplify an instruction with one or more outputs that are unused. 

(defun location-unused-p (location)
  (null (using-instructions location)))

(defgeneric simplify-instruction (instruction))

;;; By default don't do anything. 
(defmethod simplify-instruction ((instruction sicl-mir:instruction))
  nil)
  
;;; An ASSIGNMENT-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:assignment-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; If all the outputs of a GET-VALUES-INSTRUCTION are unused, then the
;;; instruction can be replaced by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:get-values-instruction))
  (when (every #'location-unused-p (outputs instruction))
    (change-class instruction 'sicl-mir:nop-instruction)))
  
;;; An ENCLOSE-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:enclose-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; A GET-ARG-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:get-arg-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; A MEMREF-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:memref-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; FIXME: add arithetic instructions here.  It is complicated because
;;; they may have more than one output, so they might be difficult to
;;; simplify.

;;; A &-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:&-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; A IOR-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:ior-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; A XOR-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:xor-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

;;; A ~-INSTRUCTION has a single output, so if it has an
;;; unused output, then all outputs are unused, and it can be replaced
;;; by a NOP-INSTRUCTION.
(defmethod simplify-instruction ((instruction sicl-mir:~-instruction))
  (change-class instruction 'sicl-mir:nop-instruction))

(defun simplify-instructions (program)
  (let ((*program* program))
    (loop for location in (find-unused-lexical-locations program)
	  do (loop for instruction in (assigning-instructions location)
		   do (simplify-instruction instruction)))))

(set-processor 'simplified-instructions 'simplify-instructions)

(add-dependencies 'simplified-instructions
		  '(location-assign-use))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Merge similar constants
;;;
;;; This simplification consists of taking all the instances of some
;;; constant C, keeping one of them and replacing each occurrence of
;;; the other instances by the one that was kept.  For now we compare
;;; constants using EQUAL, but something more precise is needed so
;;; that we can merge similar constant arrays.  We can not use EQUALP
;;; because it disregards the different between upper and lower case
;;; (for one thing).
;;;
;;; Since we might replace the EQUAL test by something more
;;; sophisticated, we are not using a hash table, and instead just a
;;; set of constantes represented as a list.

(defun merge-similar-constants (program)
  (let ((*program* program)
	(constants '()))
    (map-instructions
     (lambda (instruction)
       (loop for location in (sicl-mir:inputs instruction)
	     do (when (typep location 'sicl-mir:constant-input)
		  (pushnew location constants
			   :key #'sicl-mir:value
			   :test #'equal)))))
    (map-instructions
     (lambda (instruction)
       (loop for rest on (sicl-mir:inputs instruction)
	     do (when (typep (car rest) 'sicl-mir:constant-input)
		  (setf (car rest)
			(find (sicl-mir:value (car rest)) constants
			      :key #'sicl-mir:value
			      :test #'equal))))))))

(set-processor 'unique-constants 'merge-similar-constants)

(add-dependencies 'unique-constants
		  '(instruction-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eliminate redundant locations.
;;;
;;; We look for patterns of two lexical locations IN and OUT and two
;;; instructions I1 and I2 such that:
;;;
;;;   * I1 is an assignment instruction.
;;;
;;;   * IN is the input of I1.
;;;
;;;   * OUT is the output of I1.
;;;
;;;   * OUT is assigned to only by I1.
;;;
;;;   * OUT is used only by I2.
;;;
;;;   * In all execution paths starting at I1, I2 appears before
;;;     any instruction that assigns to IN. 
;;;
;;; The simplification consists of:
;;;
;;;   * Replacing OUT by IN as input in I2.
;;;
;;;   * Replacing I by a NOP-INSTRUCTION.
;;;
;;; FIXME: implement it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This transformation consists of removing temporary lexical
;;; locations that are known to always hold the value of a constant,
;;; and replacing occurrences of such lexical locations by a direct
;;; reference to the constant location.
;;;
;;; Later on, we reintroduce temporary lexical locations for
;;; constants, but then in a way that there is exactly one lexical
;;; location for each unique constant.

(defun remove-constant-temporaries (program)
  (let ((modify-p nil))
    (map-locations
     (lambda (location)
       (let* ((assigning-instructions (assigning-instructions location))
	      (assigning-instruction (car assigning-instructions))
	      (using-instructions (using-instructions location))
	      (using-instruction (car using-instructions)))
	 (when (and (typep location 'sicl-mir:lexical-location)
		    (= (length assigning-instructions) 1)
		    (= (length using-instructions) 1)
		    (typep assigning-instruction
			   'sicl-mir:assignment-instruction))
	   (let ((source-location (car (inputs assigning-instruction))))
	     (when (typep source-location 'sicl-mir:constant-input)
	       (when (member location (inputs using-instruction)
			     :test #'eq)
		 (setf modify-p t)
		 (nsubstitute source-location location
			      (inputs using-instruction)
			      :test #'eq))))))))
    (when modify-p
      ;; We have modified some inputs of some instructions.
      (touch program 'instruction-graph))))
  
(set-processor 'no-constant-temporaries 'remove-constant-temporaries)

(add-dependencies 'no-constant-temporaries
		  '(location-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This transformation is used when large constants have already been
;;; merged, and after backend-specific code has replaced
;;; CONSTANT-INPUT by IMMEDIATE-INPUT whenever appropriate.  Thus, it
;;; is known that all the CONSTANT-INPUTs will require memory access.
;;;
;;; The purpose of the transformation is to allow large constants to
;;; be kept in registers whenever there is a sufficient number of
;;; registers.  When there is not a sufficient number of registers, no
;;; spilling is required, because the constant is always present in
;;; the same memory location.
;;;
;;; The transformation consists of the following steps for each unique
;;; constant C:
;;;
;;;  * Create a lexical location L to hold C.
;;; 
;;;  * For every use of C that MIGHT be the FIRST use, precede the
;;;    using instruction by an ASSIGNMENT-INSTRUCTION, copying C to L,
;;;    and replace the uses of C by L in the using instruction.
;;;
;;;  * For every other use of C, replace C by L.
;;;
;;; We apply this transformation for each PROCEDURE in the program.
;;; There are several reasons for that, but mainly that we do not
;;; attempt any inter-procedural register allocation at the moment,
;;; and whenever we have several procedures in a program, it is likely
;;; that we also have procedures with indefinite extent, so that
;;; inter-procedural register allocation would not be possible anyway.
;;;
;;; Finding all the instructions that might be the first ones using C
;;; in some procedure is fairly simple.  Just do a depth-first
;;; traversal from the initial instruction.  Stop when an instruction
;;; is encountered that uses C or when an instruction that has already
;;; been traversed is encountered. 

;;; Insert an instruction before another instruction.  The new
;;; instruction will have as its only successor the existing
;;; instruction.  All occurrences of the existing instruction in the
;;; predecessors of the existing instruction will be replaced by the
;;; new instruction.
(defun insert-instruction-before (new-instruction existing-instruction)
  (loop for pred in (predecessors existing-instruction)
	do (nsubstitute new-instruction
			existing-instruction
			(successors pred)
			:test #'eq)
	   (setf (successors new-instruction)
		 (list existing-instruction))))

(defun cache-constant (constant first-instruction)
  (let ((table (make-hash-table :test #'eq))
	(temp (sicl-mir:new-temporary))
	(legitimate-users '())
	(modify-p nil))
    ;; In phase 1, we find instructions that might be the first users
    ;; of the constant and we insert an assignment instruction before
    ;; those instructions.  These new assignment instructions become
    ;; the only LEGITIMATE USERS of the constant.
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (if (member constant (sicl-mir:inputs instruction)
			     :test #'eq)
		     (let ((a (sicl-mir:make-assignment-instruction
			       constant temp instruction)))
		       (setf (gethash a table) t)
		       (setf modify-p t)
		       (push a legitimate-users)
		       (insert-instruction-before a instruction))
		     (mapc #'traverse (successors instruction))))))
      (traverse first-instruction))
    ;; In phase 2, we find instructions that use the constant, but
    ;; that are not legitimate users of it.  We replace the constant
    ;; inputs by the new temporary lexical.
    (clrhash table)
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (unless (member instruction legitimate-users :test #'eq)
		   (nsubstitute temp constant (sicl-mir:inputs instruction)
				:test #'eq))
		 (mapc #'traverse (successors instruction)))))
      (traverse first-instruction))
    modify-p))

(defun cache-constants (program)
  (let ((modify-p nil))
    (map-locations
     (lambda (location)
       (when (typep location 'sicl-mir:constant-input)
	 (loop for procedure in (procedures program)
	       for first = (initial-instruction procedure)
	       do (setf modify-p
			(or (cache-constant location first)))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'cached-constants 'cache-constants)

(add-dependencies 'cached-constants
		  '(no-constant-temporaries))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove instances of NOP-INSTRUCTION.

(defun remove-nop-instructions (program)
  (let ((modify-p nil))
    (map-instructions
     (lambda (instruction)
       (loop for rest on (successors instruction)
	     do (loop while (typep (car rest) 'sicl-mir:nop-instruction)
		      do (setf modify-p t)
			 (setf (car rest) (car (successors (car rest))))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'remove-nop-instructions 'remove-nop-instructions)

(add-dependencies 'remove-nop-instructions
		  '(instruction-info))
