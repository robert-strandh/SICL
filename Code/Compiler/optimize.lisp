(in-package #:sicl-optimize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; An instances of the CODE-OBJECT class represents a single code
;;; object.  An instruction is the INITIAL INSTRUCTION of a code
;;; object if and only if it is either the initial instruction of a
;;; PROGRAM (see below) or the INPUT of an ENCLOSE instruction.  The
;;; initial instruction of a code object does not have any
;;; predecessors.  A code object can be the input of at most one
;;; ENCLOSE instructions.  To preserve this property, inlining a code
;;; object must duplicate it, all the instructions in it, and all the
;;; lexical locations in it.
;;;
;;; The code objects of a program are nested, and the outermost code
;;; object is at NESTING DEPTH 0.  A code object B is IMMEDIATELY
;;; NESTED inside a code object A if and only if its initial
;;; instruction is the INPUT to an ENCLOSE instruction that belongs to
;;; code object A.  A code object B immediately nested inside a code
;;; object A has a nesting depth with is one greater than the nesting
;;; depth of A.
;;;
;;; An INSTRUCTION is said to be PRESENT in a code object A if it can
;;; be reached from the initial instruction of A by following
;;; successor arcs only.
;;; 
;;; An INSTRUCTION is said to BELONG to a code object A if it is
;;; present in A, but it is not present in any code object with a
;;; smaller nesting depth than that of A. 
;;; 
;;; An INSTRUCTION can be present in several code objects, but can
;;; belong to only one.  An instruction is present in more than one
;;; code objects when it belongs to some code object A, but it can
;;; also be reached from an instruction in a code object B nested
;;; inside A, so that B has a nesting depth that is greater than that
;;; of A. Such a situation is the result of a RETURN-FROM or a GO form
;;; that transfers control from one code object to an enclosing code
;;; object.
;;;
;;; A LOCATION is an explicit (present in source code) or implicit
;;; (allocated by the compiler) lexical "place" used to store local
;;; variables and temporaries.  Locations are not directly used as
;;; input or output by instructions.  Instead the are used through an
;;; intermediate object called a LEXICAL-LOCATION-INFO object.  Such
;;; an object contains the location, but also contains other
;;; information, in paricular the TYPE of the objects that the
;;; location may contain in a particular point in the program.  A
;;; location can be referred to by several different
;;; LEXICAL-LOCATION-INFO objects, which reflect the fact that a
;;; location can have different type restrictions at different points
;;; in the program, for instance as a result of a local declaration.
;;;
;;; An instruction I REFERS TO a location L if and only if a
;;; lexical-location-info object that contains L is either
;;; one of the inputs or one of the outputs of I.  
;;;
;;; A LOCATION can be referred to by several different instructions
;;; that belong to code objects at different nesting depths.  Because
;;; of the way locations are created, if a location is referred to by
;;; two different instructions belonging to two different code
;;; objects, A and B, and neither A is nested inside B nor B is nested
;;; inside A, then the location is also referred to by some code
;;; object C inside which both A and B are nested.
;;;
;;; A LOCATION L is said to be PRESENT in a code object A if and only
;;; if some instruction belonging to A refers to L.  A location L is
;;; said to BELONG to a code object A if L is present in A, and L is
;;; not present in a code object inside which A is nested.  Because of
;;; the restriction in the previous paragraph, every location belongs
;;; to some unique code object.
;;;
;;; The LEXICAL DEPTH of a code object is a quantity that is less than
;;; or equal to the NESTING depth of that code object.  We define it
;;; recursively as follows: The lexical depth of a code object A such
;;; that every location and instruction that is present in A also
;;; belongs to A is defined to be 0.  For a code object A with a
;;; location or an instruction present in it, but that belongs to a
;;; different code object B, let D be the greatest depth of any such
;;; code object B.  Then the lexical depth of A is D+1. 
;;;
;;; A SIMPLE INSTRUCTION CHAIN is a sequence of instructions, all
;;; belonging to the same code object such that every instruction in
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

(defclass code-object ()
  ((%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; Instructions that belong to this code object. 
   (%instructions :initform '() :accessor instructions)
   ;; Locations that  belong to this code object. 
   (%locations :initform '() :accessor locations)
   (%nesting-depth :initarg :nesting-depth :accessor nesting-depth)
   (%lexical-depth :initform nil :accessor lexical-depth)
   ;; The basic blocks that belong to this code object.
   (%basic-blocks :initform '() :accessor basic-blocks)))

(defun make-code-object (initial-instruction nesting-depth)
  (make-instance 'code-object
    :initial-instruction initial-instruction
    :nesting-depth nesting-depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A PROGRAM represents a top-level form together with derived
;;; knowledge of that form.  To begin with, a instance of a program
;;; contains only the initial instruction of the graph of instructions
;;; that represent nested code objects.

(defclass program ()
  ((%initial-instruction
    :initarg :initial-instruction :accessor initial-instruction)
   ;; This table maps each instruction to a list of predecessor
   ;; instructions. 
   (%predecessors :initform nil :accessor predecessors)
   ;; This table maps each instruction of the program to
   ;; the code object to which the instruction belongs.
   (%instruction-code :initform nil :accessor instruction-code)
   ;; This table maps each location of the program to the code object
   ;; to which the location belongs.
   (%location-code :initform nil :accessor location-code)
   ;; All the code objects of this program.
   (%code-objects :initform '() :accessor code-objects)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For each instruction in a complete instruction graph, determine
;;; all the nested code objects, and to which code object each
;;; instruction belongs.

(defun compute-instruction-ownership (program)
  (with-accessors ((initial-instruction initial-instruction)
		   (instruction-code instruction-code)
		   (code-objects code-objects))
      program
    (setf instruction-code (make-hash-table :test #'eq))
    (let* ((first (make-code-object initial-instruction 0))
	   (worklist (list first)))
      (push first code-objects)
      (flet
	  ((handle-single-code-object (code-object)
	     (labels
		 ((aux (instruction)
		    (when (null (gethash instruction instruction-code))
		      (setf (gethash instruction instruction-code)
			    code-object)
		      (when (typep instruction 'p2:enclose-instruction)
			(let ((new (make-code-object
				    (p2:code instruction)
				    (1+ (nesting-depth code-object)))))
			  (push new code-objects)
			  (setf worklist (append worklist (list new)))))
		      (mapc #'aux (p2:successors instruction)))))
	       (aux (initial-instruction code-object)))))
	(loop until (null worklist)
	      do (handle-single-code-object (pop worklist)))))
    (maphash (lambda (instruction code-object)
	       (push instruction (instructions code-object)))
	     instruction-code)))

(defun ensure-instruction-ownership (program)
  (when (null (instruction-code program))
    (compute-instruction-ownership program)))

(defun instruction-owner (instruction program)
  (ensure-instruction-ownership program)
  (gethash instruction (instruction-code program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the predecessors of every instruction in a program

(defun compute-predecessors (program)
  (ensure-instruction-ownership program)
  (with-accessors ((instruction-code instruction-code)
		   (predecessors predecessors))
      program
    (setf predecessors (make-hash-table :test #'eq))
    (maphash (lambda (instruction code)
	       (declare (ignore code))
	       (loop for successor in (p2:successors instruction)
		     do (push instruction (gethash successor predecessors))))
	      instruction-code)))

(defun ensure-predecessors (program)
  (when (null (predecessors program))
    (compute-predecessors program)))

(defun instruction-predecessors (instruction program)
  (ensure-predecessors program)
  (gethash instruction (predecessors program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For each location in a complete instruction graph, determine
;;; to which code object each location belongs.

(defun compute-location-ownership (program)
  (with-accessors ((location-code location-code)
		   (code-objects code-objects))
      program
    (ensure-instruction-ownership program)
    (setf location-code (make-hash-table :test #'eq))
    (let ((sorted-code
	    (sort (copy-list code-objects) #'< :key #'nesting-depth)))
      (loop for code-object in sorted-code
	    do (loop for instruction in (instructions code-object)
		     do (loop for info in (append (p2:inputs instruction)
						  (p2:outputs instruction))
			      for location = (sicl-env:location info)
			      do (when (null (gethash location location-code))
				   (setf (gethash location location-code)
					 code-object))))))
    (maphash (lambda (location code-object)
	       (push location (locations code-object)))
	     location-code)))

(defun ensure-location-ownership (program)
  (when (null (location-code program))
    (compute-location-ownership program)))

(defun location-owner (location program)
  (ensure-location-ownership program)
  (gethash location (location-code program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Determine the LEXICAl DEPTH of each code object in a program.

(defun compute-lexical-depth (code-object program)
  (setf (lexical-depth code-object)
	(loop for location in (locations code-object)
	      maximize  (let ((owner (location-owner location program)))
			  (if (eq owner code-object)
			      0
			      (1+ (lexical-depth owner)))))))

(defun compute-lexical-depths (program)
  (with-accessors ((code-objects code-objects))
      program
    (let ((sorted-code
	    (sort (copy-list code-objects) #'< :key #'nesting-depth)))
      (loop for code-object in sorted-code
	    do (compute-lexical-depth code-object program)))))

(defun ensure-lexical-depths (program)
  (when (member nil (code-objects program) :key #'lexical-depth)
    (compute-lexical-depths program)))

