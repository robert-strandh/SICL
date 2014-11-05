(in-package #:sicl-program)

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
   (%procedures :initform '() :accessor procedures)
   (%linkage-vector :initform '() :accessor linkage-vector)))

(set-processor 'instruction-graph nil)

(defmethod initialize-instance :after ((instance program) &key &allow-other-keys)
  (touch instance 'instruction-graph))

;;; This variable holds the current program that is being processed.
;;; By doing it this way, we avoid having to pass it as an argument to
;;; every function, and we also avoid the need for back pointers in
;;; every class.
(defparameter *program* nil)

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

;;; FIXME: we need to deal with non-local transfers one day.
;;; 
;;;  (remove-if (lambda (successor)
;;;	       (not (eq (owner instruction) (owner successor))))
;;;	     (successors instruction)))
(defun local-successors (instruction)
  (successors instruction))

(defun inputs (instruction)
  (sicl-mir:inputs instruction))

(defun (setf inputs) (new-inputs instruction)
  (setf (sicl-mir:inputs instruction) new-inputs))

(defun outputs (instruction)
  (sicl-mir:outputs instruction))

(defun (setf outputs) (new-outputs instruction)
  (setf (sicl-mir:outputs instruction) new-outputs))

(defun defining-instructions (datum)
  (sicl-mir:defining-instructions datum))

(defun using-instructions (datum)
  (sicl-mir:using-instructions datum))

;;; Apply a function to every instruction of the program.
(defun map-instructions (function)
  (assert (not (null *program*)))
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (instruction)
	       (unless (gethash instruction table)
		 (setf (gethash instruction table) t)
		 (funcall function instruction)
		 (loop for succ in (sicl-mir:successors instruction)
		       do (traverse succ)))))
      (traverse (initial-instruction *program*)))))

;;; Apply a function to every datum of the program.
(defun map-data (function)
  (assert (not (null *program*)))
  (let ((table (make-hash-table :test #'eq)))
    (map-instructions
     (lambda (instruction)
       (loop for datum in (append (inputs instruction) (outputs instruction))
	     do (unless (gethash datum table)
		  (setf (gethash datum table) t)
		  (funcall function datum)))))))

;;; Apply a function to every lexical location of the program.
(defun map-lexical-locations (function)
  (assert (not (null *program*)))
  (map-data
   (lambda (datum)
     (when (typep datum 'sicl-mir:lexical-location)
       (funcall function datum)))))

;;; Apply a function to every constant input of the program.
(defun map-constants (function)
  (assert (not (null *program*)))
  (map-data
   (lambda (datum)
     (when (typep datum 'sicl-mir:constant-input)
       (funcall function datum)))))

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

(defclass datum-info () ())

(defgeneric make-datum-info (datum))

(defclass lexical-location-info (datum-info)
  (;; The procedure to which this lexical location belongs.
   (%owner :initform nil :accessor owner)
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

(defclass register-location-info (datum-info)
  ())

(defmethod make-datum-info ((datum sicl-mir:register-location))
  (make-instance 'register-location-info))

(defun find-datum-info (datum)
  (assert (not (null *program*)))
  (or (gethash datum (datum-info *program*))
      (setf (gethash datum (datum-info *program*))
	    (make-datum-info datum))))

(defmethod owner ((datum sicl-mir:lexical-location))
  (owner (find-datum-info datum)))

(defmethod (setf owner) (new (datum sicl-mir:lexical-location))
  (setf (owner (find-datum-info datum))
	new))

(defmethod spill-cost ((datum sicl-mir:datum))
  (spill-cost (find-datum-info datum)))

(defmethod (setf spill-cost) (new (datum sicl-mir:datum))
  (setf (spill-cost (find-datum-info datum))
	new))

(defmethod required-register ((datum sicl-mir:datum))
  (required-register (find-datum-info datum)))

(defmethod (setf required-register) (new (datum sicl-mir:datum))
  (setf (required-register (find-datum-info datum))
	new))

(defmethod preferred-register ((datum sicl-mir:datum))
  (preferred-register (find-datum-info datum)))

(defmethod (setf preferred-register) (new (datum sicl-mir:datum))
  (setf (preferred-register (find-datum-info datum))
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
		  '(instruction-ownership))

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
    (flet ((canonicalize (constant)
	     (or (find (sicl-mir:value constant) constants
		       :key #'sicl-mir:value :test #'equal)
		 (progn (push constant constants)
			constant))))
      (map-instructions
       (lambda (instruction)
	 (let ((new-inputs
		 (loop for input in (inputs instruction)
		       collect (if (typep input 'sicl-mir:constant-input)
				   (canonicalize input)
				   input))))
	   (unless (every #'eq (inputs instruction) new-inputs)
	     (setf (inputs instruction) new-inputs))))))))

(set-processor 'unique-constants 'merge-similar-constants)

(add-dependencies 'unique-constants
		  '(instruction-info
		    backend-specific-constants))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove instances of NOP-INSTRUCTION.

(defun remove-nop-instructions (program)
  (let ((modify-p nil)
	(nop-instructions '()))
    (map-instructions
     (lambda (instruction)
       (when (typep instruction 'sicl-mir:nop-instruction)
	 (push instruction nop-instructions))))
    (unless (null nop-instructions)
      (setf modify-p t)
      (mapc #'sicl-mir:delete-instruction nop-instructions))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'remove-nop-instructions 'remove-nop-instructions)

(add-dependencies 'remove-nop-instructions
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
    (flet ((convert (input)
	     (or (convert-constant backend input) input)))
      (map-instructions
       (lambda (instruction)
	 (let ((new-inputs (mapcar #'convert (inputs instruction))))
	   (unless (every #'eq (inputs instruction) new-inputs)
	     (setf (inputs instruction) new-inputs)
	     (setf modify-p t))))))
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
;;; multiple entries in the linkage vector containing the same
;;; constant or the same global function.
;;;
;;; FIXME: consider introducting a common superclass for the three
;;; kinds of data that we handle here. 

(defun compute-linkage-vector (program)
  (let ((result '()))
    (map-data
     (lambda (datum)
       (when (and (or (typep datum 'sicl-mir:constant-input)
		      (typep datum 'sicl-mir:global-input)
		      (typep datum 'sicl-mir:load-time-input)
		      (typep datum 'sicl-mir:special-location)))
	 (pushnew datum result :test #'eq))))
    (setf (linkage-vector program) result)))

(set-processor 'linkage-vector
	       'compute-linkage-vector)

(add-dependencies 'linkage-vector
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replace constant inputs by a new temporary lexical location, and
;;; insert a LOAD-CONSTANT instruction of the constant to the lexical
;;; location before the instruction using the constant.
;;;
;;; We count on transformations such as common subexpression
;;; elimination to remove unnecessary LOAD-CONSTANT instructions
;;; later.
 
;;; Insert a LOAD-CONSTANT instruction before INSTRUCTION, with
;;; IN as its single input and OUT as its single output.
(defun insert-load-constant-before (instruction lv-index out)
  (let ((new (sicl-mir:make-load-constant-instruction
	      lv-index
	      out)))
    (sicl-mir:insert-instruction-before new instruction)))

(defun replace-constant-inputs (program)
  (let ((modify-p nil))
    (flet ((lv-offset (input)
	     (position input (linkage-vector program))))
      (map-instructions
       (lambda (instruction)
	 (unless (typep instruction 'sicl-mir:load-constant-instruction)
	   (let ((new-inputs
		   (loop for input in (inputs instruction)
			 collect (if (typep input 'sicl-mir:constant-input)
				     (let ((new (sicl-mir:new-temporary)))
				       (insert-load-constant-before
					instruction (lv-offset input) new)
					new)
				     input))))
	     (unless (every #'eq (inputs instruction) new-inputs)
	       (setf (inputs instruction) new-inputs)
	       (setf modify-p t)))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-constant-inputs 'replace-constant-inputs)

(add-dependencies 'no-constant-inputs
		  '(instruction-graph))

;;; Insert a LOAD-EXTERNAL instruction before INSTRUCTION, with
;;; IN as its single input and OUT as its single output.
(defun insert-load-global-before (instruction lv-index out)
  (let ((new (sicl-mir:make-load-global-instruction
	      lv-index
	      out)))
    (sicl-mir:insert-instruction-before new instruction)))

(defun replace-global-inputs (program)
  (let ((modify-p nil))
    (flet ((lv-offset (input)
	     (position input (linkage-vector program))))
      (map-instructions
       (lambda (instruction)
	 (unless (typep instruction 'sicl-mir:load-global-instruction)
	   (let ((new-inputs
		   (loop for input in (inputs instruction)
			 collect (if (typep input 'sicl-mir:global-input)
				     (let ((new (sicl-mir:new-temporary)))
				       (insert-load-global-before
					instruction (lv-offset input) new)
				       new)
				     input))))
	     (unless (every #'eq (inputs instruction) new-inputs)
	       (setf (inputs instruction) new-inputs)
	       (setf modify-p t)))))))
    (when modify-p
      (touch program 'instruction-graph))))

(set-processor 'no-global-inputs 'replace-global-inputs)

(add-dependencies 'no-global-inputs
		  '(instruction-graph))

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
;;; definitions/uses.  We just count the number of using and using
;;; instructions, except that if an instruction is part of a loop, we
;;; count it as 10 instead of as 1.  We could do much better here, by
;;; counting nested loops and by taking branches into account.

(defun compute-spill-costs (program)
  (let* ((loops (remove 1 (sicl-compiler-loops:kosaraju
			   (initial-instruction program) #'successors)
			:key #'length))
	 (loop-instructions (reduce #'append loops :from-end t))
	 (conflicts (compute-conflicts program)))
    (map-lexical-locations
     (lambda (location)
       (unless (eq (spill-cost location) t)
	 (setf (spill-cost location)
	       (/ (loop for inst in (append (using-instructions location)
					    (defining-instructions location))
			sum (if (member inst loop-instructions :test #'eq)
				10
				1))
		  (1+ (sicl-graph-coloring:degree location conflicts)))))))))

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
                                lexical-location new)))
              ;; We don't want to spill this one again.
              (setf (spill-cost new) t)
              (sicl-mir:insert-instruction-before assignment inst)
              (setf (inputs inst)
                    (substitute new lexical-location (inputs inst)
                                :test #'eq)))))
  (loop for inst in (defining-instructions lexical-location)
       do (unless (typep inst 'sicl-mir:assignment-instruction)
            (let* ((new (sicl-mir:new-temporary))
                   (assignment (sicl-mir:make-assignment-instruction
                                new lexical-location)))
              ;; We don't want to spill this one again.
              (setf (spill-cost new) t)
              (sicl-mir:insert-instruction-after assignment inst)
              (setf (outputs inst)
                    (substitute new lexical-location (outputs inst)
                                :test #'eq)))))
  (change-class lexical-location 'sicl-mir:dynamic-location)
  (touch *program* 'instruction-graph))


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
    (compute-spill-costs program)
    (map-lexical-locations
     (lambda (lexical-location)
       (when (and (not (eq (spill-cost lexical-location) t))
		  (or (eq cost t)
		      (< (spill-cost lexical-location) cost)))
	 (setf cost (spill-cost lexical-location))
	 (setf cheapest lexical-location))))
    cheapest))

(defun all-lexical-locations ()
  (let ((lexical-locations '()))
    (map-lexical-locations
     (lambda (lexical-location)
       (push lexical-location lexical-locations)))
    lexical-locations))
  
(defparameter *temp-counter* 0)

(defun pseudo-allocate-registers (program)
  (let ((registers (registers (backend program))))
    (loop for lexical-locations = (all-lexical-locations)
	  for conflicts = (compute-conflicts program)
	  for solution = (sicl-graph-coloring:solve registers
						    lexical-locations
						    conflicts
						    #'required-register
						    #'preferred-register)
	  while (null solution)
	  do (spill-lexical-location
	      (find-cheapest-lexical-location program))
	  finally (return solution))))

(defun valid-register-assignment-p (program)
  (let ((result t))
    (map-instructions
     (lambda (instruction)
       (when (and (typep instruction 'sicl-mir:assignment-instruction)
		  (typep (car (inputs instruction)) 'sicl-mir:dynamic-location)
		  (typep (car (outputs instruction)) 'sicl-mir:dynamic-location))
	 ;; We have an illegal assignment.
	 (let* ((new-temp (sicl-mir:new-temporary))
		(new-inst (sicl-mir:make-assignment-instruction
			   (car (inputs instruction)) new-temp)))
	   (setf (spill-cost new-temp) t)
	   (sicl-mir:insert-instruction-before new-inst instruction)
	   (setf (sicl-mir:inputs instruction) (list new-temp))
	   (setf result nil)))))
    (when (null result)
      (touch program 'instruction-graph))
    result))	 

(defun allocate-registers (program)
  (loop for solution = (pseudo-allocate-registers program)
	until (valid-register-assignment-p program)
	finally (return solution)))

(defun assign-registers (program)
  (let ((solution (allocate-registers program)))
    (loop for (lexical . reg) in solution
	  do (loop for inst in (defining-instructions lexical)
		   do (setf (outputs inst)
			    (substitute reg lexical (outputs inst)
					:test #'eq)))
	     (loop for inst in (using-instructions lexical)
		   do (setf (inputs inst)
			    (substitute reg lexical (inputs inst)
					:test #'eq))))))
		      
(set-processor 'register-assignment
	       'assign-registers)

(add-dependencies 'register-assignment
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove redundant assignment instructions.
;;;
;;; An assignment instruction is redundant when it has the same input
;;; and output.

(defun remove-redundant-assignment-instructions (program)
  (declare (ignore program))
  (let ((redundant-instructions '()))
    (map-instructions
     (lambda (instruction)
       (when (and (typep instruction 'sicl-mir:assignment-instruction)
		  (eq (car (inputs instruction))
		      (car (outputs instruction))))
	 (push instruction redundant-instructions))))
    (mapc #'sicl-mir:delete-instruction redundant-instructions)))

(set-processor 'no-redundant-assignments
	       'remove-redundant-assignment-instructions)

(add-dependencies 'no-redundant-assignments
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We need to find out how much stack space to allocate and to what
;;; stack location each spilled register belongs.  For that reason, we
;;; need to collect all the dynamic locations of the program.

(defun collect-dynamic-locations (program)
  (declare (ignore program))
  (let ((dynamic-locations '()))
    (map-instructions
     (lambda (instruction)
       (let ((data (append (sicl-mir:inputs instruction)
			   (sicl-mir:outputs instruction))))
	 (loop for datum in data
	       do (when (typep datum 'sicl-mir:dynamic-location)
		    (pushnew datum dynamic-locations :test #'eq))))))
    dynamic-locations))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type inference.

(defun type-inference (program)
  (let* ((initial-instruction (initial-instruction program))
	 (type-info (sicl-compiler:type-inference initial-instruction)))
    (sicl-compiler:trim-instruction-graph initial-instruction type-info))
  (touch program 'instruction-graph))

(set-processor 'type-inference
	       'type-inference)

(add-dependencies 'type-inference
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove unused locations.
;;;
;;; This transformation checks whether there are any lexical locations
;;; that are only assigned to and never used.  For any such location,
;;; it removes any assignment instruction having that location as an
;;; output.  Removing such an assignment instruction might create
;;; another instance of an unused lexical location (the input of the
;;; assignment instruction), and in that case the new location is
;;; added to the worklist.
;;;
;;; For now, we only remove ASSIGNMENT-INSTRUCTIONs with the unused
;;; location as its output.  Later, we may consider removing other
;;; kinds of instructions as well, but this has to be done with some
;;; precautions.

(defun remove-unused-locations (program)
  (let ((worklist '())
	(modified-p nil))
    (map-lexical-locations
     (lambda (lexical-location)
       (when (null (sicl-mir:using-instructions lexical-location))
	 (pushnew lexical-location worklist :test #'eq))))
    (loop until (null worklist)
	  do (loop with location = (pop worklist)
		   for instruction in (sicl-mir:defining-instructions location)
		   do (when (typep instruction 'sicl-mir:assignment-instruction)
			(setf modified-p t)
			(let ((input (car (sicl-mir:inputs instruction))))
			  (sicl-mir:delete-instruction instruction)
			  ;; Deleting the instruction will remove it from
			  ;; the using instructions of its inputs.
			  (when (and (typep input 'sicl-mir:lexical-location)
				     (null (sicl-mir:using-instructions input)))
			    (push input worklist))))))
    (when modified-p
      (touch program 'instruction-graph))))

(set-processor 'no-unused-locations
	       'remove-unused-locations)

(add-dependencies 'no-unused-locations
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Remove redundant temporaries.
;;;
;;; This little transformation is subsumed by global value numbering,
;;; but until we have implemented global value numbering, this
;;; transformation comes in handy.
;;;
;;; For the purpose of this transformation, a REDUNDANT TEMPORARY is
;;; defined to be a lexical location L with the following properties.
;;; L has a single defining instruction and a single using
;;; instruction.  The defining instruction must be an
;;; ASSIGNMENT-INSTRUCTION (call it I0), but the using instruction can
;;; be any type of instruction (call it In).  Call the single input of
;;; I0 X.  X can be a lexical location or a constant input.  If X is a
;;; lexical location, then there must also be a simple execution path
;;; I0 -> I1 -> ... -> In such that each instruction Ii in the path
;;; has I(i+1) as its only successor, and none of the instructions in
;;; the path (except possibly In) may have X in its outputs. 
;;;
;;; If such a lexical location L can be found, then L can be replaced
;;; by X in In and the instruction I0 can be removed. 
;;;
;;; An important observation is that applying this transformation to a
;;; single location L can not remove the conditions for the
;;; transformation to be applied to some other location.

;;; Return true if and only if there is a straight execution path from
;;; INSTRUCTION1 to INSTRUCTION2 such that each instruction in the
;;; path (except possibly INSTRUCTION2) has a single successor and
;;; such that LEXICAL-LOCATION is not an output of any of the
;;; instruction in the path (except possibly INSTRUCTION2).
(defun simple-path-p (instruction1 instruction2 lexical-location)
  (loop for instruction = instruction1 then successor
	for successors = (sicl-mir:successors instruction)
	for successor = (car successors)
	until (eq instruction instruction2)
	do (when (or (/= (length successors) 1)
		     (member lexical-location (sicl-mir:outputs instruction)
			     :test #'eq))
	     (return-from simple-path-p nil)))
  t)

(defun remove-redundant-temporaries (program)
  (let ((modified-p nil))
    (map-lexical-locations
     (lambda (location)
       (let ((defining-instructions (sicl-mir:defining-instructions location))
	     (using-instructions (sicl-mir:using-instructions location)))
	 (when (and (= 1 (length defining-instructions))
		    (= 1 (length using-instructions))
		    (typep (car defining-instructions)
			   'sicl-mir:assignment-instruction))
	   (let* ((defining (car defining-instructions))
		  (using (car using-instructions))
		  (input (car (sicl-mir:inputs defining))))
	     (when (or (typep input 'sicl-mir:constant-input)
		       (typep input 'sicl-mir:immediate-input)
		       (and (typep input 'sicl-mir:lexical-location)
			    (simple-path-p defining using input)))
	       (setf modified-p t)
	       (sicl-mir:delete-instruction defining)
	       (setf (sicl-mir:inputs using)
		     (substitute input location (sicl-mir:inputs using)))))))))
    (when modified-p
      (touch program 'instruction-graph))))

(set-processor 'no-redundant-temporaries
	       'remove-redundant-temporaries)

(add-dependencies 'no-redundant-temporaries
		  '(instruction-graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Do some initial transformations.

(defun initial-transformations (program)
  ;; There are typically unused locations in the initial
  ;; program. 
  (make program 'no-unused-locations)
  (make program 'no-redundant-temporaries)
  (make program 'type-inference)
  ;; Type inference may create unused locations, so apply
  ;; this transformation again.
  (make program 'no-unused-locations)
  (make program 'no-redundant-temporaries)
  (make program 'remove-nop-instructions)
  (make program 'backend-specific-constants)
  (make program 'unique-constants)
  (make program 'linkage-vector)
  (make program 'no-constant-inputs)
  (make program 'no-global-inputs)
  (make program 'lir)
  (make program 'no-unused-locations)
  (make program 'register-assignment)
  (make program 'no-redundant-assignments)
;;  (make program 'instruction-ownership)
;;  (make program 'location-ownership)
  )
