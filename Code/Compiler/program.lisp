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

(defgeneric predecessors (instruction-or-info))
(defgeneric (setf predecessors) (new instruction-or-info))
(defgeneric owner (instruction-or-info))
(defgeneric (setf owner) (new instruction-or-info))
(defgeneric basic-block (instruction))
(defgeneric (setf basic-block) (new instruction))

(defclass instruction-info ()
  (;; The list of predecessor instructions of this instruction. 
   (%predecessors :initform '() :accessor predecessors)
   ;; The procedure to which this instruction belongs.
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

(defmethod predecessors ((instruction sicl-mir:instruction))
  (predecessors (find-instruction-info instruction)))

(defmethod (setf predecessors) (new (instruction sicl-mir:instruction))
  (setf (predecessors (find-instruction-info instruction))
	new))

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
   (%assigning-instructions :initform nil :accessor assigning-instructions)))

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
;;; Determine the LEXICAl DEPTH of each procedure in a program.

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
		  '(instruction-ownership
		    predecessors))

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
;;; Find loops.
;;;
;;; Recall that a loop is present whenever there is a back arc in the
;;; flow graph.  An arc is a back arc when the HEAD of the arc
;;; DOMINATES the TAIL of the arc.  The head of the arc is the INITIAL
;;; block of the loop and the tail of the arc is the FINAL block of
;;; the loop.
;;;
;;; Thus a LOOP is a SET of basic blocks with an INITIAL basic block I
;;; and a FINAL basic block F, such that I dominates F.  A block B is
;;; in the set if and only if B is an ancestor of F and B is dominated
;;; by I.
;;;
;;; To find all the blocks in a loop given a back arc, if suffices to
;;; recursively trace predecessors of F until I is reached.  Notice
;;; though that there might be nested loops, so that care must be
;;; taken when predecessors are traced to avoid tracing blocks
;;; multiple times.

;;; Since we don't have an explicit representation of arcs in the flow
;;; graph (nor in the instruction graph), we represent a back arc as a
;;; pair of basic blocks, (F . I) where F is the final block and I is
;;; the inital block of the loop, or, equivalently, where F is the
;;; tail of the arc and I is the head of the arc.
(defun find-back-arcs-in-procedure (procedure)
  (loop for block in (basic-blocks procedure)
	for successors = (mapcar #'basic-block (successors (final block)))
	append (loop for successor in successors
		     when (member successor (dominators block))
		       collect (cons block successor))))

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
	(constants '()))
    (map-constants
     (lambda (constant)
       (pushnew constant constants
		:key #'sicl-mir:value
		:test #'equal)))
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
			:test #'eq))
  (setf (successors new-instruction)
	(list existing-instruction)))

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
		 (setf (gethash instruction table) t)
		 (if (and (member constant (sicl-mir:inputs instruction)
				  :test #'eq))
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
  (let ((modify-p nil)
	(inputs '()))
    (map-instructions
     (lambda (instruction)
       (loop for input in (inputs instruction)
	     do (when (and (typep input 'sicl-mir:constant-input)
			   (not (member input inputs :test #'eq)))
		  (push input inputs)
		  (loop for procedure in (procedures program)
			for first = (initial-instruction procedure)
			do (setf modify-p
				 (or (cache-constant input first) modify-p)))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'cached-constants 'cache-constants)

(add-dependencies 'cached-constants
		  '(procedures
		    datum-assign-use
		    no-redundant-temporaries))

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
