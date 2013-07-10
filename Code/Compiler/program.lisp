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
;;;;
;;;; We follow the terminology of Steven S Muchnick, in that a FLOW
;;;; GRAPH is a graph of BASIC BLOCKS. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A BACKEND is an object that contains various backend-specific data
;;; such as the physical registers of the machine, etc.  It is also
;;; used as dispatch for certain generic functions. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REGISTERS.
;;;
;;; Return a list of the physical registers of the backend. 

(defgeneric registers (backend))

(defclass backend () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A PROGRAM represents a top-level form together with derived
;;; knowledge of that form.  To begin with, an instance of a program
;;; contains only the initial instruction of the graph of instructions
;;; that represent nested procedures.

(defclass program ()
  (;; The backend for which this program is intended.
   (%backend :initarg :backend :initform nil :reader backend)
   (%timestamps
    :initform (make-hash-table :test #'eq) :reader timestamps)
   (%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; This table maps each instruction of the program to an
   ;; instruction-info instance.
   (%instruction-info
    :initform (make-hash-table :test #'eq) :accessor instruction-info)
   ;; This table maps each datum of the program to a datum-info
   ;; instance.
   (%datum-info
    :initform (make-hash-table :test #'eq) :accessor datum-info)
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

;;; Apply a function to every instruction of the program.
(defun map-instructions (function)
  (assert (not (null *program*)))
  (maphash (lambda (instruction info)
	     (declare (ignore info))
	     (funcall function instruction))
	   (instruction-info *program*)))

;;; Apply a function to every datum of the program.
(defun map-data (function)
  (assert (not (null *program*)))
  (maphash (lambda (datum info)
	     (declare (ignore info))
	     (funcall function datum))
	   (datum-info *program*)))

;;; Apply a function to every lexical location of the program.
(defun map-lexical-locations (function)
  (assert (not (null *program*)))
  (maphash (lambda (datum info)
	     (declare (ignore info))
	     (when (typep datum 'sicl-mir:lexical-location)
	       (funcall function datum)))
	   (datum-info *program*)))

;;; Apply a function to every constant input of the program.
(defun map-constants (function)
  (assert (not (null *program*)))
  (maphash (lambda (datum info)
	     (declare (ignore info))
	     (when (typep datum 'sicl-mir:constant-input)
	       (funcall function datum)))
	   (datum-info *program*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction info.
;;;
;;; We need a lot of information about an instruction.  Rather than
;;; storing that information in the instruction itself, we store it in
;;; an INSTRUCTION-INFO object.  There is one such object for each
;;; instruction, we put it in a hash table with the instruction as a
;;; key.

(defgeneric owner (instruction-or-info))
(defgeneric (setf owner) (new instruction-or-info))
(defgeneric basic-block (instruction))
(defgeneric (setf basic-block) (new instruction))

(defclass instruction-info ()
  (;; The procedure to which this instruction belongs.
   (%owner :initform nil :accessor owner)
   ;; The basic block to which this instruction belongs.
   (%basic-block :initform nil :accessor basic-block)))

(defun find-instruction-info (instruction)
  (assert (not (null *program*)))
  (multiple-value-bind (info present-p)
      (gethash instruction (instruction-info *program*))
    (unless present-p
      (error "no instruction-info for instruction ~s" instruction))
    info))

(defmethod owner ((instruction sicl-mir:instruction))
  (owner (find-instruction-info instruction)))

(defmethod (setf owner) (new (instruction sicl-mir:instruction))
  (setf (owner (find-instruction-info instruction))
	new))

(defmethod basic-block ((instruction sicl-mir:instruction))
  (basic-block (find-instruction-info instruction)))

(defmethod (setf basic-block) (new (instruction sicl-mir:instruction))
  (setf (basic-block (find-instruction-info instruction))
	new))

(defgeneric predecessors (object)
  (:method ((object sicl-mir:instruction))
    (sicl-mir:predecessors object)))

(defgeneric (setf predecessors) (new-predecessors object)
  (:method (new-predecessors (object sicl-mir:instruction))
    (setf (sicl-mir:predecessors object) new-predecessors)))

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
;;; Datum info.
;;;
;;; As with instructions, We need a som information about every datum
;;; of the program.  Again, rather than storing that information in
;;; the datum itself, we store it in an DATUM-INFO object.  There is
;;; one such object for each datum, we put it in a hash table with the
;;; datum as a key.
;;;
;;; Contrary to an instruction info object, we need different types of
;;; information for different types of data, such as lexical
;;; locations, global inputs, constants, etc.  For that reason, we
;;; define different subclasses of datum-info.

(defclass datum-info ()
  (;; A list of all instructions that have this datum as an input.
   (%using-instructions :initform nil :accessor using-instructions)))

(defgeneric make-datum-info (datum))

(defclass lexical-location-info (datum-info)
  (;; The procedure to which this lexical location belongs.
   (%owner :initform nil :accessor owner)
   ;; A list of all instructions that have this location as an output.
   (%assigning-instructions :initform nil :accessor assigning-instructions)
   ;; If a backend requires this lexical location to be in a
   ;; particular register when it is not spilled, then it can set this
   ;; slot to the required register.  If there is no required register
   ;; for this lexical location, then the value of this slot is nil.
   ;; The register allocator uses this information to assign a
   ;; register a priori, and excluding this lexical location from
   ;; further consideration.  The backend typically uses this method
   ;; for callee-saved register; either they are spilled to the stack,
   ;; or they stay in a particular register.
   (%required-register :initform nil :accessor required-register)
   ;; If a backend determins that it is advantageous for this lexical
   ;; location to be in a particular register, then it sets this slot
   ;; to the preferred register.  The effect is that when the register
   ;; allocator assigns a register to this location, rather than
   ;; taking any available register, if the preferred register is
   ;; available, that is the one it selects.  The backend typically
   ;; uses this method for lexical locations representing parameters
   ;; and arguments that are passed in registers.  
   (%preferred-register :initform nil :accessor preferred-register)
   ;; The spill cost is computed once all new lexical locations have
   ;; been added by the backend.  The spill cost is computed as the
   ;; number of estimated definitions/uses of the lexical location
   ;; during the execution of the program divided by the number of
   ;; conflicts it participates in.  A value of NIL means that the
   ;; spill const has not been computed yet.  A spill cost of T means
   ;; "infinity" so that this lexical should never be spilled, and its
   ;; spill cost should never be altered.
   (%spill-cost :initform nil :accessor spill-cost)))

(defmethod make-datum-info ((datum sicl-mir:lexical-location))
  (make-instance 'lexical-location-info))

(defclass constant-input-info (datum-info)
  ())

(defmethod make-datum-info ((datum sicl-mir:constant-input))
  (make-instance 'constant-input-info))

(defclass global-input-info (datum-info)
  ())

(defmethod make-datum-info ((datum sicl-mir:global-input))
  (make-instance 'global-input-info))

(defclass word-input-info (datum-info)
  ())

(defmethod make-datum-info ((datum sicl-mir:word-input))
  (make-instance 'word-input-info))

(defclass immediate-input-info (datum-info)
  ())

(defmethod make-datum-info ((datum sicl-mir:immediate-input))
  (make-instance 'immediate-input-info))

(defun find-datum-info (datum)
  (assert (not (null *program*)))
  (multiple-value-bind (info present-p)
      (gethash datum (datum-info *program*))
    (unless present-p
      (error "no datum-info for datum ~s" datum))
    info))

(defmethod owner ((datum sicl-mir:lexical-location))
  (owner (find-datum-info datum)))

(defmethod (setf owner) (new (datum sicl-mir:lexical-location))
  (setf (owner (find-datum-info datum))
	new))

(defmethod assigning-instructions ((datum sicl-mir:lexical-location))
  (assigning-instructions (find-datum-info datum)))

(defmethod (setf assigning-instructions)
    (new (datum sicl-mir:lexical-location))
  (setf (assigning-instructions (find-datum-info datum))
	new))

(defmethod using-instructions ((datum sicl-mir:datum))
  (using-instructions (find-datum-info datum)))

(defmethod (setf using-instructions) (new (datum sicl-mir:datum))
  (setf (using-instructions (find-datum-info datum))
	new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class PROCEDURE.

(defclass procedure ()
  ((%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; Instructions that belong to this procedure. 
   (%instructions :initform '() :accessor instructions)
   ;; Lexical locations that  belong to this procedure. 
   (%lexical-locations :initform '() :accessor lexical-locations)
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
    (setf procedures '())
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
;;; Compute datum info.
;;;
;;; For each datum in the program, create a datum-info instance and
;;; put it in the datum-info hash table.

(defun compute-datum-info (program)
  (let ((*program* program))
    (with-accessors ((datum-info datum-info))
	program
      (clrhash datum-info)
      (map-instructions
       (lambda (instruction)
	 (loop for datum in (append (inputs instruction)
				    (outputs instruction))
	       do (when (null (gethash datum datum-info))
		    (setf (gethash datum datum-info)
			  (make-datum-info datum)))))))))

(set-processor 'datum-info 'compute-datum-info )

;;; We use MAP-INSTRUCTIONS for computing the data, which requries the
;;; DATUM-INFO to have been computed.
(add-dependencies 'datum-info
		  '(instruction-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute location ownership.

(defun compute-location-ownership-for-procedure (procedure)
  (loop for instruction in (instructions procedure)
	do (loop for datum in (append (inputs instruction)
				      (outputs instruction))
		 do (when (and (typep datum 'sicl-mir:lexical-location)
			       (null (owner datum)))
		      (setf (owner datum) procedure)))))

(defun compute-location-ownership (program)
  (let ((*program* program))
    (with-accessors ((procedures procedures))
	program
      (let ((sorted-code
	      (sort (copy-list procedures) #'< :key #'nesting-depth)))
	(loop for procedure in sorted-code
	      do (compute-location-ownership-for-procedure procedure)))
      (map-lexical-locations
       (lambda (lexical-location)
	 (push lexical-location
	       (lexical-locations (owner lexical-location))))))))

(set-processor 'location-ownership 'compute-location-ownership)

(add-dependencies 'location-ownership
		  '(instruction-ownership
		    datum-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the assigning and using instructions of each datum.

(defun compute-datum-assign-use (program)
  (let ((*program* program))
    ;; Start by clearing the assigning and using instructions
    ;; of each datum.
    (map-data
     (lambda (datum)
       (setf (using-instructions datum) '())))
    (map-lexical-locations
     (lambda (lexical-location)
       (setf (assigning-instructions lexical-location) '())))
    ;; Next, for each instruction, add it to the list of assigning
    ;; instructions for all its outputs, and addit to the list of
    ;; using instructions for all its inputs.
    (map-instructions
     (lambda (instruction)
       (loop for input in (inputs instruction)
	     do (push instruction (using-instructions input)))
       (loop for output in (outputs instruction)
	     do (push instruction (assigning-instructions output)))))))

(set-processor 'datum-assign-use 'compute-datum-assign-use)

(add-dependencies 'datum-assign-use
		  '(datum-info
		    instruction-ownership))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the LEXICAL DEPTH of each procedure in a program.

(defun compute-lexical-depth (procedure)
  (setf (lexical-depth procedure)
	(loop for lexical-location in (lexical-locations procedure)
	      maximize  (let ((owner (owner lexical-location)))
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
;;; Basic blocks.

(defclass basic-block ()
  ((;; The first instruction in the basic block.
    %initial :initarg :initial :accessor initial)
   ;; The last instruction in the basic block.
   (%final :initarg :final :accessor final)
   ;; The basic blocks that dominate this block (not necessarily
   ;; immediately, and not strictly).
   (%dominators :initform '() :accessor dominators)))

(defun compute-basic-blocks-for-procedure (procedure)
  (with-accessors ((instructions instructions)
		   (basic-blocks basic-blocks))
      procedure
    (setf basic-blocks '())
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
		 (let ((b (make-instance 'basic-block
			    :initial initial
			    :final final)))
		   (loop for ins = initial then (car (successors ins))
			 do (setf (basic-block ins) b)
			 until (eq ins final))
		   b))))
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
;;; Compute the dominance between basic blocks.
;;;
;;; The dominance relation is indicated as a property of each block
;;; containing the basic blocks that dominate it.

(defmethod predecessors ((obj basic-block))
  (mapcar #'basic-block (predecessors (initial obj))))

(defun compute-dominance-for-procedure (procedure)
  ;; First clear the existing information.
  (loop for block in (basic-blocks procedure)
	do (setf (dominators block) '()))
  ;; Now, make sure the initial block is first on the list of
  ;; basic block of this procedure.  It simplifies algorithms later.
  (let ((initial-block (basic-block (initial-instruction procedure))))
    (setf (basic-blocks procedure)
	  (cons initial-block
		(remove initial-block (basic-blocks procedure) :test #'eq))))
  ;; For the purpose of this algorithm, we assign a unique number to
  ;; the basic blocks.  We need a way to translate from a basic block
  ;; to its unique number, and from a number to the corresponding
  ;; basic block.  For the first mapping, we use a hash table, and for
  ;; the second one a vector of basic blocks.
  (let ((blocks (coerce (basic-blocks procedure) 'vector))
	(numbers (make-hash-table :test #'eq))
	(dominators (make-array (length (basic-blocks procedure)))))
    ;; Determine the unique number of each block.
    (loop for block across blocks
	  for i from 0
	  do (setf (gethash block numbers) i))
    ;; The initial block should have a set containing only itself, so
    ;; its associated bit vector should have a single 1 in the
    ;; position corresponding to itself.  We know that the initial
    ;; block is block number 0 from making that happen above.
    (let ((set (make-array (length blocks)
			   :element-type 'bit :initial-element 0)))
      (setf (sbit set 0) 1)
      (setf (aref dominators 0) set))
    ;; Initialize every bit vector except the one of the initial block
    ;; to a vector of all 1s, meaning that for now we think that every
    ;; block is dominated by every other block.
    (loop for i from 1 below (length dominators)
	  do (setf (aref dominators i)
		   (make-array (length blocks)
			       :element-type 'bit
			       :initial-element 1)))
    (loop with change = nil
	  for i from 1 below (length dominators)
	  for block = (aref blocks i)
	  do (let ((temp (make-array (length dominators)
				     :element-type 'bit
				     :initial-element 1)))
	       (loop for predecessor in (predecessors block)
		     for predecessor-number = (gethash predecessor numbers)
		     do (bit-and temp
				 (aref dominators predecessor-number)
				 temp))
	       (setf (sbit temp i) 1)
	       (unless (equal temp (aref dominators i))
		 (setf change t)
		 (setf (aref dominators i) temp)))
	  while change)
    (loop for i from 0 below (length dominators)
	  for b1  across blocks
	  do (loop for j from 0 below (length dominators)
		   for b2 across blocks
		   do (when (= (sbit (aref dominators i) j) 1)
			(push b2 (dominators b1)))))))

(defun compute-dominance (program)
  (loop for procedure in (procedures program)
	do (compute-dominance-for-procedure procedure)))
  
(set-processor 'dominance 'compute-dominance)

(add-dependencies 'dominance
		  '(basic-blocks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find unused lexical locations of a program.

(defun find-unused-lexical-locations (program)
  (let ((*program* program)
	(result '()))
    (map-lexical-locations
     (lambda (location)
       (when (null (using-instructions location))
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
		  '(datum-assign-use))

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
	(constants '())
	(modify-p nil))
    (map-constants
     (lambda (constant)
       (pushnew constant constants
		:key #'sicl-mir:value
		:test #'equal)))
    (map-instructions
     (lambda (instruction)
       (loop for rest on (sicl-mir:inputs instruction)
	     do (when (typep (car rest) 'sicl-mir:constant-input)
		  (unless (member (car rest) constants :test #'eq)
		    (setf modify-p t)
		    (setf (car rest)
			  (find (sicl-mir:value (car rest)) constants
				:key #'sicl-mir:value
				:test #'equal)))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'unique-constants 'merge-similar-constants)

(add-dependencies 'unique-constants
		  '(instruction-info
		    backend-specific-constants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eliminate redundant lexical locations.
;;;
;;; We look for patterns of a location IN (which might be any datum),
;;; a lexical locations OUT, and an instructions I such that:
;;;
;;;   * I is an assignment instruction.
;;;
;;;   * IN is the input of I.
;;;
;;;   * OUT is the output of I.
;;;
;;;   * OUT is assigned to only by I.
;;;
;;;   * In all execution paths starting at I, OUT is used before IN is
;;;     assigned to.
;;;
;;; The simplification consists of:
;;;
;;;   * Replacing OUT by IN as input in all instructions. 
;;;
;;;   * Replacing I by a NOP-INSTRUCTION.
;;;
;;; This is a dataflow problem that can be solved by fixpoint
;;; iterations.  Before some instruction I, let T mean that there
;;; exists an execution path starting at I where IN is assigned to
;;; before OUT is used, i.e. the negation of what we want.  Start off
;;; optimistically with the value NIL before each instruction.
;;; 
;;; For each instructions I that does not use OUT but that assigns to
;;; IN, set the value to T, trace backward through its predecessors,
;;; propagating the T until either:
;;;
;;;   * An instruction is reached that already has the value T.
;;;  
;;;   * An instruction I is found that assigns to IN or uses OUT.  If
;;;     I does not assign to OUT, then it either has already been
;;;     processed, or it will later.  If it does assign to OUT, then
;;;     propatagtion should stop.

(defun handle-one-candidate (instruction in out)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (or (gethash instruction table)
			   (member in (outputs instruction) :test #'eq)
			   (member out (inputs instruction) :test #'eq))
		 (setf (gethash instruction table) t)
		 (mapc #'traverse (predecessors instruction)))))
      (map-instructions
       (lambda (instruction)
	 (when (and (not (member out (inputs instruction) :test #'eq))
		    (member in (outputs instruction) :test #'eq))
	   (traverse instruction)))))
    (if (gethash instruction table)
	nil
	(progn
	  (map-instructions
	   (lambda (instruction)
	     (nsubstitute in out (inputs instruction))))
	  (change-class instruction 'sicl-mir:nop-instruction)
	  t))))
	 
(defun eliminate-redundant-temporaries (program)
  (let ((modify-p nil))
    (map-instructions
     (lambda (instruction)
       (when (typep instruction 'sicl-mir:assignment-instruction)
	 (let ((out (car (outputs instruction))))
	   (when (and (typep out 'sicl-mir:lexical-location)
		      (= (length (assigning-instructions out)) 1))
	     (let ((in (car (inputs instruction))))
	       (setf modify-p
		     (or (handle-one-candidate instruction in out)
			 modify-p))))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-redundant-temporaries 'eliminate-redundant-temporaries)

(add-dependencies 'no-redundant-temporaries
		  '(instruction-info
		    datum-assign-use))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The purpose of this transformation is to identify FUNCALL
;;; instructions where the function called is known not to return.
;;; For now, we only handle calls to CL:ERROR, but in the future, we
;;; might handle calls to any CLHS function declared to return a value
;;; of type NULL.  
;;;
;;; While this transformation might not seem worthwhile, in fact it
;;; is, because it cuts off certain paths in the instruction graph
;;; which decreases the number of basic blocks, and makes it easier
;;; for the register allocator to do its job.
;;;
;;; For this transformation to work, the first input to the FUNCALL
;;; instruction must be a GLOBAL-INPUT (where the NAME property is
;;; CL:ERROR).  Therefore, It will only work right after redundant
;;; temporaries have been eleminated.

(defun eliminate-error-successors (program)
  (let ((modify-p nil))
    (map-instructions
     (lambda (instruction)
       (when (and (typep instruction 'sicl-mir:funcall-instruction)
		  (let ((fun (car (inputs instruction))))
		    (and (typep fun 'sicl-mir:global-input)
			 (eq (sicl-mir:name fun) 'error))))
	 (setf modify-p t)
	 (setf (successors instruction) '()))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-error-successors 'eliminate-error-successors)

(add-dependencies 'no-error-successors
		  '(instruction-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The purpose of this transformation is to introduce immediate data
;;; whenever possible.  Since determining when an immidiate datum is
;;; possible depends on the backend, this function calls a generic
;;; function CONVERT-CONSTANT with the backend and the constant.  The
;;; function CONVERT-CONSTANT returns returns either NIL (meaning,
;;; leave the datum as it is), or a different datum that should be
;;; used instead.  The return value is a freshly allocated datum.
;;;
;;; This transformation should be applied before we attempt to
;;; uniqueify constants, because it may introduce duplicate constants
;;; that need to be uniqueified later.  
;;;
;;; After this tranformation has been applied, there are no WORD-INPUT
;;; data left in the program, because every such datum has been
;;; converted to either a CONSTANT-INPUT or an IMMEDIATE-INPUT.  

(defgeneric convert-constant (backend constant))

(defmethod convert-constant ((backend null) constant)
  nil)

(defun convert-constants-according-to-backend (program)
  (let ((backend (backend program))
	(modify-p nil))
    (flet ((maybe-replace (cons)
	     (let ((replacement (convert-constant backend (car cons))))
	       (unless (null replacement)
		 (setf (car cons) replacement)
		 (setf modify-p t)))))
      (map-instructions
       (lambda (instruction)
	 (mapl #'maybe-replace (inputs instruction)))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'backend-specific-constants
	       'convert-constants-according-to-backend)

(add-dependencies 'backend-specific-constants
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine indices for all data that need to be allocated there,
;;; i.e., data of type CONSTANT-INPUT, GLOBAL-INPUT, LOAD-TIME-INPUT,
;;; and SPECIAL-LOCATION.
;;;
;;; We do not expect the number of of such data to be huge, so it is
;;; not justified to use a hash table.  For that reason, we use an
;;; ALIST, where the CAR of each element is the datum and the CDR of
;;; each element is the unique index that was allocated for that
;;; datum.
;;;
;;; Before this computation is done, it is advantageous to uniquify
;;; the inputs for which an index is to be computed, so as to avoid
;;; multiple entries in the linkage vector containg the same constant
;;; or the same global function. 
;;;
;;; FIXME: consider introducting a common superclass for the three
;;; kinds of data that we handle here. 

(defun compute-linkage-vector-indices (program)
  (declare (ignore program))
  (let ((index 0)
	(result '()))
    (map-data
     (lambda (datum)
       (when (and (or (typep datum 'sicl-mir:constant-input)
		      (typep datum 'sicl-mir:global-input)
		      (typep datum 'sicl-mir:load-time-input)
		      (typep datum 'sicl-mir:special-location))
		  (null (assoc datum result :test #'eq)))
	 (push (cons datum index) result)
	 (incf index))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replace constant inputs by a new temporary lexical location, and
;;; insert a LOAD-CONSTANT instruction of the constant to the lexical
;;; location before the instruction using the constant.
;;;
;;; We count on transformations such as common subexpression
;;; elimination to remove unnecessary LOAD-CONSTANT instructions
;;; later.

;;; Insert an instruction before INSTRUCTION.  The new instruction
;;; already has INSTRUCTION as its successor.
(defun insert-instruction-before-instruction (new instruction)
  (loop for pred in (predecessors instruction)
	do (nsubstitute new
			instruction
			(successors pred)
			:test #'eq)))

;;; Insert an assignment instruction before INSTRUCTION, assigning IN
;;; to OUT.
(defun insert-assignment-before (instruction in out)
  (let ((new (sicl-mir:make-assignment-instruction in out instruction)))
    (insert-instruction-before-instruction new instruction)))

;;; Insert a LOAD-CONSTANT instruction before INSTRUCTION, with
;;; IN as its single input and OUT as its single output.
(defun insert-load-constant-before (instruction in out)
  (let ((new (sicl-mir:make-load-constant-instruction
	      (list in) out instruction)))
    (insert-instruction-before-instruction new instruction)))

(defun replace-constant-inputs (program)
  (let ((modify-p nil))
    (map-instructions
     (lambda (instruction)
       (unless (typep instruction 'sicl-mir:load-constant-instruction)
	 (loop for rest on (sicl-mir:inputs instruction)
	       do (when (typep (car rest) 'sicl-mir:constant-input)
		    (let ((new (sicl-mir:new-temporary)))
		      (insert-load-constant-before
		       instruction (car rest) new)
		      (setf (car rest) new))
		    (setf modify-p t))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-constant-inputs 'replace-constant-inputs)

(add-dependencies 'no-constant-inputs
		  '(dominance))

;;; Insert a LOAD-EXTERNAL instruction before INSTRUCTION, with
;;; IN as its single input and OUT as its single output.
(defun insert-load-global-before (instruction in out)
  (let ((new (sicl-mir:make-load-global-instruction
	      (list in) out instruction)))
    (loop for pred in (predecessors instruction)
	  do (nsubstitute new
			  instruction
			  (successors pred)
			  :test #'eq))))

(defun replace-global-inputs (program)
  (let ((modify-p nil))
    (map-instructions
     (lambda (instruction)
       (unless (typep instruction 'sicl-mir:load-global-instruction)
	 (loop for rest on (sicl-mir:inputs instruction)
	       do (when (typep (car rest) 'sicl-mir:global-input)
		    (let ((new (sicl-mir:new-temporary)))
		      (insert-load-global-before
		       instruction (car rest) new)
		      (setf (car rest) new))
		    (setf modify-p t))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-global-inputs 'replace-global-inputs)

(add-dependencies 'no-global-inputs
		  '(dominance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Convert from MIR to LIR.

(defgeneric convert-to-lir (backend initial-instruction))

(defun convert-from-mir-to-lir (program)
  (convert-to-lir (backend program) (initial-instruction program))
  (touch program 'instruction-graph))

(set-processor 'lir 'convert-from-mir-to-lir)

(add-dependencies 'lir
		  '(no-constant-inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Liveness analysis
;;;
;;; An item I (lexical location or register) is LIVE at some point P
;;; in a program if and only if there exists an execution path from P
;;; to the end of the procedure on which I is used before it is
;;; assigned to.
;;;
;;; Liveness is going to be represented as two sets of items for each
;;; instructions; the items that are live BEFORE the instruction, and
;;; the items that are live AFTER the instruction.
;;;
;;; Computing liveness is a dataflow problem that is best computed
;;; from the end of the program to the beginning of the program.
;;; Given the set A of live items AFTER some instruction I, the set B
;;; of items live BEFORE the instruction is computed as A intersected
;;; with the items written to by I and then unioned by the items used
;;; by I.  Given the set Bi of live items live before each successor i
;;; of some instruction I, the set A of live items after I is computed
;;; as the union of all the Bi.
;;;
;;; Initially the sets A(I) and B(I) for each instruction are empty.
;;; The computation is iterated until a fixpoint is reached.  In fact,
;;; it is unnecessary to maintain an explicit representation of both
;;; the A and the B set.  The B set is enough. 
;;;
;;; The literature suggests using bitvectors for this computation,
;;; where a particular item has a unique index in the bitvector.
;;; However, to avoid storing the unique indices in the items, we
;;; would have to maintain a hash table and a vector for the mapping
;;; from items to indices and from indices to items respectively.
;;; Instead we attempt to maintain the sets as Lisp lists.  If
;;; performance becomes a problem, we might revisit this decision.
;;;
;;; The literature also suggests doing this computation on basic
;;; blocks.  The reason is that a basic block can be made to behave
;;; much like a single instruction, in that it defines and uses some
;;; items.  It can be a big gain in performance to perform the
;;; iterative computation on basic blocks until a fixpoint is reached,
;;; and then compute a simple loop for each basic block.  Again, we
;;; are willing to bet that performance is no longer a problem, and
;;; that we can get away with doing this computation on individual
;;; instructions.  And of course, we might rethink that decision as
;;; well if performance becomes a problem.
;;;
;;; We have a further complication compared to the situation described
;;; in the literature, namely that our programs are structured into
;;; nested procedures, and that the successor of some instruction can
;;; be in an enclosing procedure.  However, liveness analysis is only
;;; concerned with variables with DYNAMIC EXTENT.  Variables with
;;; indefinite extent are not allocated in register nor on the stack,
;;; but in the static environment.  A call to an enclosed procedure
;;; must save all callee-saved registers that might be trashed by that
;;; procedure, and those registers must be restored as part of the
;;; non-local transfer.  As far as liveness analysis is concerned
;;; then, an instruction I in a procedure P where every successor Ji
;;; of I is in Qi /= P can be treated as a RETURN, i.e. the end of the
;;; execution of P.  As far as Q is concerned, a predecessor I of J
;;; can be ignored.

(defun set-equal (set1 set2)
  (and (subsetp set1 set2 :test #'eq) (subsetp set2 set1 :test #'eq)))

(defun compute-liveness (program)
  (let ((*program* program))
    (make program 'instruction-ownership)
    (let (;; An entry in the table associates an instruction with a list of
	  ;; the items that are live before it.
	  (btable (make-hash-table :test #'eq))
	  ;; An entry in the table associates an instruction with a list of
	  ;; the items that are live after it.
	  (atable (make-hash-table :test #'eq)))
      (labels
	  ((traverse (instruction)
	     (let ((live '()))
	       ;; First compute the union of the items that are live
	       ;; before each of the successors of INSTRUCTION.
	       (loop for successor in (successors instruction)
		     do (setf live
			      (union live (gethash successor btable))))
	       (multiple-value-bind (current present-p)
		   (gethash instruction atable)
		 (unless (and present-p (set-equal live current))
		   ;; Something has changed.  Propagate!
		   (setf (gethash instruction atable) live)
		   ;; Remove from the set the items that are written
		   ;; by INSTRUCTION.
		   (loop for output in (outputs instruction)
			 do (setf live (remove output live :test #'eq)))
		   ;; Add to the set the items used by INSTRUCTION
		   ;; that are registers or lexical locations.
		   (loop for input in (inputs instruction)
			 do (when (or (typep input 'sicl-mir:lexical-location)
				      (typep input 'sicl-mir:register-location))
			      (pushnew input live :test #'eq)))
		   (setf (gethash instruction btable) live)
		   (loop for pred in (predecessors instruction)
			 do (when (eq (owner pred) (owner instruction))
			      (traverse pred))))))))
	(map-instructions
	 (lambda (instruction)
	   (when (or (null (successors instruction))
		     (every (lambda (successor)
			      (not (eq (owner successor) (owner instruction))))
			    (successors instruction)))
	     (traverse instruction)))))
      atable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute a set of conflicts for the register allocator.  Recall
;;; that two items generate a conflict when one is live at the point
;;; where the other is written to.
;;;
;;; We do not want to include conflicts between two registers in our
;;; set.  Nor do we want multiple copies of some conflict.  We have to
;;; be careful because the relation is symmetric, so that if (L1 . L2)
;;; is a conflict in the set, we do not want to add (L2 . L1) because
;;; it is the same conflict.

(defun same-conflict-p (c1 c2)
  (or (and (eq (car c1) (car c2))
	   (eq (cdr c1) (cdr c2)))
      (and (eq (car c1) (cdr c2))
	   (eq (cdr c1) (car c2)))))

(defun compute-conflicts (program)
  (let ((*program* program)
	(conflicts '()))
    (let ((atable (compute-liveness program)))
      (map-instructions
       (lambda (instruction)
	 (loop for output in (outputs instruction)
	       do (loop for live in (gethash instruction atable)
			do (when (or (typep output 'sicl-mir:lexical-location)
				     (typep live 'sicl-mir:lexical-location))
			     (when (typep live 'sicl-mir:register-location)
			       (rotatef live output))
			     (pushnew (cons live output) conflicts
				      :test #'same-conflict-p)))))))
    conflicts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the reaching definitions of a program.  
;;;
;;; A DEFINITION is a CONS of an INSTRUCTION and a LEXICAL-LOCATION
;;; that is an output of that instruction.  A definition is said to
;;; REACH an instruction I if and only if there exists an execution
;;; path from the definition to I in which the lexical location is not
;;; the output of any other instruction.
;;;
;;; The REACHING DEFINITIONS of an instruction I is a SET of
;;; DEFINITIONS that reach I.  
;;;
;;; The problem of computing the reaching definitions of a procedure
;;; is a so-called FORWARD DATAFLOW problem, in that it can be solved
;;; by fixpoint iteration, starting from the initial instruction and
;;; following successors until a fixpoint is reached.  

(defun compute-reaching-definitions-for-procedure (procedure)
  (let ((atable (make-hash-table :test #'eq))
	(btable (make-hash-table :test #'eq)))
    (flet ((same-set-p (s1 s2)
	     (and (subsetp s1 s2 :test #'equal)
		  (subsetp s2 s1 :test #'equal))))
      (flet ((lexical-outputs (instruction)
	       (remove-if-not (lambda (output)
				(typep output 'sicl-mir:lexical-location))
			      (sicl-mir:outputs instruction))))
	(flet ((b-to-a (instruction)
		 (let* ((before (gethash instruction btable))
			(locations (lexical-outputs instruction))
			(stripped (remove-if (lambda (x)
					       (member (cdr x) locations
						       :test #'eq))
					     before))
			(new-definitions (mapcar (lambda (location)
						   (cons instruction location))
						 locations))
			(after (append new-definitions stripped)))
		   (setf (gethash instruction atable) after))))
	  ;; A function that returns only the successors of an
	  ;; instruction I that have the same owner as I does.
	  (flet ((sp-successors (instruction)
		   (remove-if-not (lambda (succ)
				    (eq (owner instruction) (owner succ)))
				  (successors instruction))))
	    (labels ((traverse (instruction)
		       (multiple-value-bind (value present-p)
			   (gethash instruction btable)
			 (let* ((new (apply
				      #'concatenate 'list
				      (mapcar (lambda (inst)
						(gethash inst atable))
					      (predecessors instruction))))
				(unique (remove-duplicates new :test #'equal)))
			   (when (or (not present-p)
				     (not (same-set-p value unique)))
			     (setf (gethash instruction btable) new)
			     (b-to-a instruction)
			     (mapc #'traverse (sp-successors instruction)))))))
	      (traverse (initial-instruction procedure)))))))
    btable))

(defun compute-reaching-definitions (program)
  (let ((*program* program))
    (loop for procedure in (procedures program)
	  append
	  (loop for instruction being each hash-key
		using (hash-value definitions)
		of (compute-reaching-definitions-for-procedure procedure)
		collect (cons instruction definitions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute DU-chains.
;;;
;;; A UD-chain is a not a chain at all, but rather a mapping that
;;; takes a DEFINITION and returns a set of INSTRUCTIONS that might
;;; use that definition.  Recall that a definition is a CONS of an
;;; INSTRUCTION and a LEXICAL-LOCATION.  

(defun compute-du-chains (program)
  (let* (;; Start by computing the reaching definitions.  Recall that
	 ;; we obtain a list for each instruction in the program.  The
	 ;; list has the instruction as its CAR and the definitions
	 ;; that reach that definition as its CDR.
	 (reaching-definitions (compute-reaching-definitions program))
	 ;; To obtain every definition in the program, one way is to
	 ;; uniquify the lists of the definitions returned by the
	 ;; reaching definitions.  It's a bit wasteful since there are
	 ;; a lot of duplicates, but who is counting? 
	 (definitions (remove-duplicates
		       (reduce #'append
			       (mapcar #'cdr reaching-definitions)
			       :from-end t)
		       :test #'equal)))
    (mapcar (lambda (definition)
	      (cons definition
		    (loop for rd in reaching-definitions
			  when (and (member (cdr definition)
					    (sicl-mir:inputs (car rd))
					    :test #'eq)
				    (member definition (cdr rd)
					    :test #'equal))
			    collect (car rd))))
	    definitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute web sets.
;;;
;;; Two DU-chains are said to INTERSECT if and only if:
;;;
;;;   * They concern the same lexical location, and
;;;
;;;   * They have at least one using instruction in common.
;;;
;;; A WEB is a maximal set of intersecting DU-chains.  The concept of
;;; a web is an important one, because when there are several distinct
;;; webs for one particular lexical location, then this is a result of
;;; the programmer having used the same variable name for several
;;; distinct purposes.  By recognizing such distinct usages, we can
;;; improve register allocation because the number of conflicts in the
;;; allocation graph decreases then.
;;;
;;; This function returns a list of as many elements as there are
;;; lexical locations in the program.  Each element of the list is a
;;; list of webs.  A web is represented as a list of three elements:
;;;
;;;   * The first element is the lexical location concerned by the
;;;     web.
;;;
;;;   * The second element is a list of defining instructions.
;;;
;;;   * The third element is a list of using instructions.
;;; 
;;; Most sublists of the top-level list will contain a single element,
;;; in particular sublists for temporary variables allocated by the
;;; compiler.
;;;
;;; Recall that a DU-chain is represented as a list with the CAR being
;;; a CONS cell of an defining instruction and a lexical location, and
;;; the CDR being a list of using instructions. 

;;; From a non-empty list of DU-chains concerning the same lexical
;;; location, exctract a single web consisting of all the DU-chains in
;;; the list that instersect the first DU-chain of the list.  Return
;;; two values: the web that was calculated, and a list of the
;;; remaining DU-chains that do not instersect the first one.
(defun extract-one-web (du-chains)
  (let* ((first (car du-chains))
	 (rest (cdr du-chains))
	 (web (list (cdar first) (list (caar first)) (cdr first))))
    (loop for chain = (find-if-not
		       (lambda (chain)
			 (null (intersection (cdr chain) (third web)
					     :test #'eq)))
		       rest)
	  until (null chain)
	  do (setf rest (remove chain rest :test #'eq))
	     (push (caar chain) (second web))
	     (setf (third web) (union (third web) (cdr chain) :test #'eq)))
    (values web rest)))
  
;;; Given a (possibly empty) list of DU-chains concerning the same
;;; lexical location, return a list of distinct webs correspodning to
;;; those DU-chains.
;;;
;;; We do not expect there to be very many different du-chains
;;; concerning one particular location, so we can afford to use a
;;; recusive solution.
(defun chains-to-webs (du-chains)
  (if (null du-chains)
      '()
      (multiple-value-bind (web rest)
	  (extract-one-web du-chains)
	(cons web (chains-to-webs rest)))))

(defun compute-web-sets (program)
  (let ((du-chains (compute-du-chains program))
	(webs '()))
    (loop until (null du-chains)
	  for lexical-location = (cdar (car du-chains))
	  do (let ((related-chains
		     (remove lexical-location du-chains
			     :key #'cdar :test-not #'eq)))
	       (setf du-chains
		     (set-difference du-chains related-chains :test #'eq))
	       (push (chains-to-webs related-chains) webs)))
    webs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Uniquify web.
;;;
;;; Given a web, allocate a new lexical location and replace the
;;; existing lexical location in all the defining and all the using
;;; instructions by the new one.
;;; 
;;; The purpose of this transformation is to make sure that each
;;; lexical location of the program is concerned by a single web, so
;;; that we can use lexical locations as the unit for register
;;; allocation.  When we discover that we have more than one distinct
;;; web for a particular lexical location, we uniquify all but one of
;;; them.  For most lexical locations, there is a single web, so for
;;; most lexical locations, this function will never be called.
;;;
;;; Recall that a web is represented as a list of three elements: The
;;; lexical location concerned by the web, a list of defining
;;; instructions, and a list of using instructions.

(defun uniquify-web (web)
  (let* ((old (first web))
	 (name (sicl-mir:name (car old)))
	 (new (sicl-mir:make-lexical-location name)))
    (mapc (lambda (instruction)
	    (nsubstitute new old (sicl-mir:outputs instruction)))
	  (second web))
    (mapc (lambda (instruction)
	    (nsubstitute new old (sicl-mir:inputs instruction)))
	  (third web))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Uniquify webs.
;;;
;;; Make sure that every lexical location in a program corresponds to
;;; a unique web.

(defun uniquify-webs (program)
  (let ((*program* program))
    ;; Recall that the web sets computed by COMPUTE-WEB-SETS is a list
    ;; that has as many elements as there are lexical locations in the
    ;; program.  Each element of the top-level list is a set of webs
    ;; represented as a list, all concerning the same lexical
    ;; location.  We uniqify all but one (the first) web in the list.
    (mapc (lambda (web-set)
	    (unless (null (cdr web-set))
	      (touch program 'instruction-graph))
	    (mapc #'uniquify-web (cdr web-set)))
	  (compute-web-sets program)))
  nil)

(set-processor 'unique-webs 'uniquify-webs)

(add-dependencies 'unique-webs
		  '(predecesors
		    procedures
		    instruction-ownership))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Depth-first pre-order and post-order traversal of a flowchart.

(defun depth-first-pre-order (initial-instruction)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for succ in (successors instruction)
		       do (traverse succ))
		 (push instruction result))))
      (traverse initial-instruction))
    result))

(defun depth-first-post-order (initial-instruction)
  (reverse (depth-first-pre-order initial-instruction)))

(defun backward-depth-first-pre-order (initial-instruction)
  (let ((table (make-hash-table :test #'eq))
	(result '()))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for pred in (predecessors instruction)
		       do (traverse pred))
		 (push instruction result))))
      (traverse initial-instruction))
    result))
  
(defun backward-depth-first-post-order (initial-instruction)
  (reverse (backward-depth-first-pre-order initial-instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find loops.  
;;;
;;; A loop can only be the result of a TAGBODY form.  Whenever there
;;; is a non-trivial strongly connected component in the instruction
;;; graph, we have a loop.  The only restriction on loops in Common
;;; Lisp is that the TAGBODY form introduces a unique instruction that
;;; dominates every other instruction in the strongly connected
;;; component.  Another way of saying the same thing is that it is not
;;; possible to transfer control from the outside of a TAGBODY form
;;; into it (we shall se later that we can ignore control arcs from a
;;; nested procedure).  
;;;
;;; If we compute a dominance tree of all the instructions in a
;;; strongly connected component S, then a nested loop shows up as a
;;; (sub) strongly connected component immediately dominated by some
;;; other instruction than the one that dominates the entire component
;;; S.  Let us say that I0 is the instruction that dominates every
;;; instruction in the strongly connected component.  Let I10, I11,
;;; ..., I1n be the instructions in S that are immediately dominated
;;; by I0.  If we remove I0 and I10, I11, ... I1n from S and the
;;; remaining instructions contain a strongly connected component T,
;;; this means that there is a nested loop, and since I0 is not the
;;; instruction that immediately dominates them, there must be some
;;; other instruction I that does.  
;;;
;;; Notice that a nested loop detected this way may or may not be the
;;; result of a nested TAGBODY form.  It could also appear because the
;;; tags and the GO forms of the top-level loop form such a nested
;;; loop "by accident".  But we still want to count it as a nested
;;; loop.  And it is unlikely that such a nested loop would appear by
;;; accident.  
;;;
;;; Finally, it is possible that a nested loop that appears by
;;; accident, i.e., that is not the result of a nested TAGBODY form,
;;; can not be detected by the method describe above, because there is
;;; no restriction preventing a GO into the middle of it.  As a result
;;; we might have a real nested loop, but since the instructions in it
;;; are all directly dominated by I0, then we won't find it.

;;; Kosaraju's algoritm is a simple algorithm for finding the strongly
;;; connected components of an instruction graph.  The return value is
;;; a set (represented as a list) of strongly connected components,
;;; each represented as a list of instructions.  A trivial component
;;; will be represented as a list of a single element.
(defun kosaraju (initial-instruction)
  (let ((pre (depth-first-pre-order initial-instruction))
	(table (make-hash-table :test #'eq))
	(result '())
	(temp '()))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (loop for pred in (predecessors instruction)
		       do (traverse pred))
		 (push instruction temp))))
      (loop until (null pre)
	    for initial = (pop pre)
	    unless (gethash initial table)
	      do (setf temp '())
		 (traverse initial)
		 (push temp result))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the spill cost of every lexical location.
;;;
;;; The spill cost is computed once all new lexical locations have
;;; been added by the backend.  The spill cost is computed as the
;;; number of estimated definitions/uses of the lexical location during
;;; the execution of the program divided by the number of conflicts it
;;; participates in.
;;;
;;; The conflicts are computed in the same way as for the register
;;; allocator.  For now, we use a very crude estimate of the number of
;;; definitions/uses.  We just count the number of assigning and using
;;; instructions, except that if an instruction is part of a loop, we
;;; count it as 10 instead of as 1.  We could do much better here, by
;;; counting nested loops and by taking branches into account.

(defun compute-spill-costs (program)
  (let* ((loops (remove 1 (kosaraju (initial-instruction program))
			:test #'length))
	 (loop-instructions (reduce #'append loops :from-end t))
	 (conflicts (compute-conflicts program)))
    (map-lexical-locations
     (lambda (location)
       (unless (eq (spill-cost location) t)
	 (setf (spill-cost location)
	       (/ (loop for inst in (append (using-instructions location)
					    (assigning-instructions location))
			sum (if (member inst loop-instructions :test #'eq)
				10
				1))
		  (sicl-graph-coloring:degree location conflicts))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Spill a lexical location.
;;;
;;; We look at all the instructions that define or use the location L,
;;; and we distinguish between assignment instructions and other
;;; instructions (called COMPUTATIONAL).
;;;
;;; For every computational instruction I that USES L, we create a new
;;; lexical location M.  We insert an assignment instruction A before
;;; I.  A assigns L to M.  L is replaced by M as an input of I.  For
;;; every computational instruction I that DEFINES L, we create a new
;;; lexical location M.  We insert an assignment instruction A before
;;; I.  A assigns M to L.  L is replaced by M as an output of I.
;;;
;;; Assignment instructions are left alone for now.  

(defun spill-lexical-location (lexical-location)
  (loop for inst in (using-instructions lexical-location)
	do (unless (typep inst 'sicl-mir:assignment-instruction)
	     (let* ((new (sicl-mir:new-temporary))
		    (assignment (sicl-mir:make-assignment-instruction
				 lexical-location new inst)))
	       ;; We don't want to spill this one again.
	       (setf (spill-cost new) t)
	       (sicl-mir:insert-instruction-before assignment inst)
	       (nsubstitute new lexical-location (inputs inst)
			    :test #'eq))))
  (loop for inst in (assigning-instructions lexical-location)
	do (unless (typep inst 'sicl-mir:assignment-instruction)
	     (let* ((new (sicl-mir:new-temporary))
		    (assignment (sicl-mir:make-assignment-instruction
				 lexical-location new inst)))
	       ;; We don't want to spill this one again.
	       (setf (spill-cost new) t)
	       (sicl-mir:insert-instruction-after assignment inst)
	       (nsubstitute new lexical-location (outputs inst)
			    :test #'eq)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Register allocation.
;;;
;;; The graph-coloring module tries to find a solution that assigns a
;;; register to every lexical location.  That might fail because of
;;; too many conflicts.
;;;
;;; Here, we call the graph-coloring solver repeatedly until a
;;; solution is found, and each time with one fewer lexical locations,
;;; i.e. the one we decided to spill because it had the lowest spill
;;; cost.
;;;
;;; However, the solution that we obtain this way might not be valid,
;;; because it may contain an assignment instruction that assigns
;;; between two stack locations.  When that happens, we add a new
;;; lexical location with infinite spill cost and two assignment
;;; instructions that assign between the two stack locations, using
;;; the new lexical location as an intermediate.  Now, since we have
;;; introduced more lexical locations, we need to solve from the
;;; beginning again.

(defun find-cheapest-lexical-location (program)
  (let ((*program* program)
	(cheapest nil)
	(cost t))
    (map-lexical-locations
     (lambda (lexical-location)
       (when (and (not (eq (spill-cost lexical-location) t))
		  (or (eq cost t)
		      (< (spill-cost lexical-location) cost)))
	 (setf cost (spill-cost lexical-location))
	 (setf cheapest lexical-location))))
    cheapest))

(defun pseudo-allocate-registers (program)
  (make program 'datum-assign-use)
  (let ((lexical-locations '())
	(registers (registers (backend program)))
	(conflicts (compute-conflicts program)))
    (map-lexical-locations
     (lambda (lexical-location)
       (push lexical-location lexical-locations)))
    (loop for solution = (sicl-graph-coloring:solve registers
						    lexical-locations
						    conflicts
						    #'required-register
						    #'preferred-register)
	  while (null solution)
	  do (spill-lexical-location
	      (find-cheapest-lexical-location program))
	  finally (return solution))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Do some initial transformations.

(defun initial-transformations (program)
  (make program 'unique-webs)
  (make program 'no-redundant-temporaries)
  (make program 'no-error-successors)
  (make program 'simplified-instructions)
  (make program 'remove-nop-instructions)
  (make program 'unique-constants)
  (make program 'basic-blocks)
  (make program 'dominance)
  (make program 'no-constant-inputs)
  (make program 'no-global-inputs))
  
