(in-package #:cleavir-ir)

;;;; Data are used as inputs to and outputs from instructions. 
;;;;
;;;; The possible types of data that can be found in a MIR
;;;; program depend on the stage of translation.  Globally speaking,
;;;; there are three kinds of data.  The first kind represents
;;;; small constants that can occur directly in an instruction stream.
;;;; The second kind represent more complex constants that must be
;;;; represented separately.  The third kind represents a LEXICAL
;;;; LOCATION and is an explicit (present in source code) or implicit
;;;; (allocated by the compiler) lexical "place" used to store local
;;;; variables and temporaries.  Lexical locations are important
;;;; because we must decide where to put them, and that decision can
;;;; be different in different parts of the code.
;;;;
;;;; An instruction I REFERS TO a lexical location L if and only if
;;;; L is either one of the inputs or one of the outputs of I.
;;;;
;;;; A lexical location can be referred to by several different
;;;; instructions that belong to procedures at different nesting
;;;; depths.  Because of the way lexical locations are created, if a
;;;; lexical location is referred to by two different instructions
;;;; belonging to two different procedures, P and Q, and neither P is
;;;; nested inside Q nor is Q nested inside P, then the lexical
;;;; location is also referred to by some instruction belonging to a
;;;; procedure C inside which both A and B are nested.
;;;;
;;;; A lexical location L is said to be PRESENT in a procedure P if
;;;; and only if some instruction belonging to P refers to L.  A
;;;; lexical location L is said to BELONG to a procedure P if L is
;;;; present in P, and L is not present in a procedure inside which P
;;;; is nested.  Because of the restriction in the previous paragraph,
;;;; every lexical location belongs to some unique procedure.  The
;;;; procedure P to which a lexical location belongs is called the
;;;; OWNER of the lexical location.
;;;;
;;;; The LEXICAL DEPTH of a procedure is a quantity that is less than
;;;; or equal to the NESTING depth of that procedure.  We define it
;;;; recursively as follows: The lexical depth of a procedure P such
;;;; that every lexical location that is present in P also belongs to
;;;; P is defined to be 0.  For a procedure A with a lexical location
;;;; present in it, but that belongs to a different procedure Q, let D
;;;; be the greatest depth of any such procedure Q.  Then the lexical
;;;; depth of P is D+1.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class IMMEDIATE-INPUT.
;;;
;;; The IMMEDIATE-INPUT datum corresponds to a raw machine interger
;;; that is considered sufficiently small that it can occur directly
;;; in the instruction stream.  The machine integer is represented in
;;; the instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character. 
;;;
;;; Data of this type are introduced by backend-specific code,
;;; because whether or not some datum can be represented as immediate
;;; input depends on the backend. 

(defclass immediate-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-immediate-input (value)
  (make-instance 'immediate-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class WORD-INPUT.
;;;
;;; The WORD-INPUT datum corresponds to a raw machine interger that
;;; may or may not be sufficiently small to occur directly in the
;;; instruction stream.  The machine integer is represented in the
;;; instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character.
;;;
;;; Data of this type are introduced in early compilation stages
;;; when a WORD-AST is compiled, and in later stages when
;;; backend-specific code determins that a Lisp constant has a
;;; representation as a machine word, typically a character constant.
;;; Notice that such as character constant may or may not be possible
;;; to represent as an immediate input, depending on the Unicode code
;;; point of the character, and depending on the magnitude of
;;; immediates that the bacend can handle.

(defclass word-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-word-input (value)
  (make-instance 'word-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class CONSTANT-INPUT.
;;;
;;; In later compilation stages, this datum is eliminated.  If the
;;; constant can be encoded as an immediate value, then an
;;; IMMEDIATE-INPUT is used instead.  If not, then the constant is
;;; allocated in the linkage vector of the code object, and an
;;; EXTERNAL-INPUT is used instead.

(defclass constant-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-constant-input (value)
  (make-instance 'constant-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class LEXICAL-LOCATION.
;;;
;;; This datum class is used when the initial MIR program is created
;;; for any lexical variable.  
;;; 
;;; In later compilation stages, this datum is replaced by more
;;; specific locations as a result of further analyses of the program.

(defclass lexical-location (datum)
  ((%name :initarg :name :reader name)))

(defun make-lexical-location (name)
  (make-instance 'lexical-location
    :name name))

(defmethod print-object ((object lexical-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class DYNAMIC-LEXICAL-LOCATION.
;;;
;;; This datum is a special case of a LEXICAL-LOCATION.  It is used
;;; for locations that are only referred to within a single function,
;;; so that there is no possible capture.  A location of this type can
;;; be allocated in a register or on the stack.

(defclass dynamic-lexical-location (lexical-location)
  ())

(defun make-dynamic-lexical-location (name)
  (make-instance 'dynamic-lexical-location
    :name name))

;;; Generate a new lexical location
(defun new-temporary ()
  (make-dynamic-lexical-location (gensym)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class INDEFINITE-LEXICAL-LOCATION.
;;;
;;; This datum is a special case of a LEXICAL-LOCATION.  It is used
;;; for locations that are only referred to within several functions,
;;; so that the location has indefinite extent.  Whether a location of
;;; this type can be allocated in a register or on the stack depends
;;; on further analyses.  To be safe, client code might want to turn
;;; this location into a STATIC-LOCATION so that it is allocated in
;;; the static environment.

(defclass indefinite-lexical-location (lexical-location)
  ())

(defun make-indefinite-lexical-location (name)
  (make-instance 'indefinite-lexical-location
    :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class LOAD-TIME-INPUT.
;;;
;;; This datum typically corresponds to a LOAD-TIME-VALUE in source
;;; code.  However, in the file compiler, we also convert each
;;; CONSTANT-INPUT that can not be turned into an IMMEDIATE-INPUT into
;;; a LOAD-TIME-INPUT datum.  
;;;
;;; A datum of this type contains code to be run when the compiled
;;; file is loaded, and the result of that code becomes the value of
;;; some entry in the linkage vector.

(defclass load-time-input (datum)
  (;; The first instruction of a MIR instruction graph that, when
   ;; called, returns the value to put into the linkage vector.
   (%initial-instruction
    :initarg :initial-instruction
    :reader initial-instruction)))

(defun make-load-time-input (initial-instruction)
  (make-instance 'load-time-input
    :initial-instruction initial-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class EXTERNAL-INPUT.
;;;
;;; This datum corresponds to an index in the linkage vector of a code
;;; object.  Later compilation stages typically replace a
;;; CONSTANT-INPUT that can not be an IMMEDIATE-INPUT by an
;;; EXTERNAL-INPUT.

(defclass external-input (datum)
  ((%value :initarg :value :reader value)))

(defun make-external-input (value)
  (make-instance 'external-input
    :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class REGISTER-LOCATION.
;;;
;;; This datum corresponds to a processor register.  It is
;;; introduced by the register allocation phase.

(defclass register-location (datum)
  ((%name :initarg :name :reader name)))

(defun make-register-location (name)
  (make-instance 'register-location
    :name name))

(defmethod print-object ((object register-location) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class STATIC-LOCATION.
;;;
;;; This datum corresponds to a places in the static runtime
;;; envirionment.  That environment is organized in LAYERS, with each
;;; layer being a vector of values.  

(defclass static-location (datum)
  ((%layer :initarg :layer :reader layer)
   (%index :initarg :index :reader index)))

(defun make-static-location (layer index)
  (make-instance 'static-location
    :layer layer
    :index index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Datum class DYNAMIC-LOCATION.
;;;
;;; This datum corresponds to places in the stack frame where the
;;; compiler might decide to put objects with dynamic extent.  

(defclass dynamic-location (datum)
  ((%index :initform 0 :initarg :index :reader index)))

(defun make-dynamic-location (index)
  (make-instance 'dynamic-location
    :index index))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for successor count.

;;; Mixin class for instructions with no successors. 
(defclass no-successors-mixin () ())

;;; Mixin class for instructions with a single successor.
(defclass one-successor-mixin () ())

;;; Mixin class for instructions with tow successors.
(defclass two-successors-mixin () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin classes for boxing instructions.

;;; Mixin class for instructions that box unboxed data. 
(defclass box-instruction-mixin () ())

(defgeneric box-instruction-p (instruction))

(defmethod box-instruction-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod box-instruction-p ((instruction box-instruction-mixin))
  (declare (ignorable instruction))
  t)

;;; Mixin class for instructions that ubox boxed data. 
(defclass unbox-instruction-mixin () ())

(defgeneric unbox-instruction-p (instruction))

(defmethod unbox-instruction-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod unbox-instruction-p ((instruction unbox-instruction-mixin))
  (declare (ignorable instruction))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mixin class for instructions that have no side effects.

(defclass side-effect-free-mixin () ())

(defgeneric side-effect-free-p (instruction))

(defmethod side-effect-free-p (instruction)
  (declare (ignore instruction))
  nil)

(defmethod side-effect-free-p ((instruction side-effect-free-mixin))
  (declare (ignorable instruction))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for Common Lisp operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENTER-INSTRUCTION.
;;;
;;; This instruction encapsulates all the implementation-specific
;;; machinery involved in verifying the argument count and parsing the
;;; arguments.  It has a single successor.

(defclass enter-instruction (instruction one-successor-mixin)
  ((%lambda-list :initarg :lambda-list :accessor lambda-list)))

(defun make-enter-instruction (lambda-list successor)
  (let* ((outputs (loop for item in lambda-list
			append (cond ((member item lambda-list-keywords) '())
				     ((consp item) item)
				     (t (list item))))))
    (make-instance 'enter-instruction
      :lambda-list lambda-list
      :outputs outputs
      :successors (list successor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOP-INSTRUCTION.

(defclass nop-instruction (instruction one-successor-mixin)
  ())

(defun make-nop-instruction (successors)
  (make-instance 'nop-instruction
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ASSIGNMENT-INSTRUCTION.

(defclass assignment-instruction (instruction one-successor-mixin)
  ())

(defun make-assignment-instruction
    (input output &optional (successor nil successor-p))
  (make-instance 'assignment-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FUNCALL-INSTRUCTION.

(defclass funcall-instruction (instruction one-successor-mixin)
  ())

(defun make-funcall-instruction (inputs &optional (successor nil successor-p))
  (make-instance 'funcall-instruction
    :inputs inputs
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TAILCALL-INSTRUCTION.

(defclass tailcall-instruction (instruction no-successors-mixin)
  ())

(defun make-tailcall-instruction (inputs)
  (make-instance 'tailcall-instruction
    :inputs inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-INSTRUCTION.

(defclass return-instruction (instruction no-successors-mixin)
  ())

(defun make-return-instruction (inputs)
  (make-instance 'return-instruction
    :inputs inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENCLOSE-INSTRUCTION.

(defclass enclose-instruction (instruction one-successor-mixin)
  ((%code :initarg :code :accessor code)))  

(defun make-enclose-instruction (output successor code)
  (make-instance 'enclose-instruction
    :outputs (list output)
    :successors (list successor)
    :code code))

(defmethod clone-instruction :around ((instruction enclose-instruction))
  (let ((new (call-next-method)))
    (setf (code new) (code instruction))
    new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TYPEQ-INSTRUCTION.
;;;
;;; This instruction takes one input, namely a datum for which a type
;;; should be tested.  
;;;
;;; As a result of various transformations of the instruction graph,
;;; this instruction will either be eliminated (because we can
;;; determine statically the result of the test), or it will be
;;; replaced by a call to TYPEP.  When it is replaced by a call to
;;; TYPEP, we use the constant input as the second argument to TYPEP.

(defclass typeq-instruction (instruction two-successors-mixin)
  ((%value-type :initarg :value-type :reader value-type)))

(defun make-typeq-instruction (input successors value-type)
  (make-instance 'typeq-instruction
    :inputs (list input)
    :successors successors
    :value-type value-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction THE-INSTRUCTION.
;;;
;;; This instruction is similar to the TYPEQ-INSTRUCTION.  It is
;;; different in that it has only a single successor and no error
;;; branch.  Operationally, it has no effect.  But it informs the type
;;; inference machinery that the input is of a particular type. 

(defclass the-instruction (instruction one-successor-mixin)
  ((%value-type :initarg :value-type :reader value-type)))

(defun make-the-instruction (input successor)
  (make-instance 'the-instruction
    :inputs (list input)
    :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CATCH-INSTRUCTION.
;;;
;;; This instruction is used to mark the stack to be an exit point.
;;; It takes a single input and it has a single successor.  It has no
;;; outputs.  The effect of the instruction is to push an entry onto
;;; the dynamic environment that contains the value of the input to
;;; the instruction and the current stack.
;;;
;;; To implement the Common Lisp CATCH special operator, the entire
;;; CATCH form would be placed in a thunk that can not be inlined
;;; (because the return address must be explicit).  Inside that thunk,
;;; the CATCH-INSTRUCTION would be used to mark capture the stack at
;;; that point.  The THROW special operator would search the dynamic
;;; environment for the frame, and use the return address stored in it. 
;;;
;;; The CATCH-INSTRUCTION can also be used to implement lexical
;;; non-local control transfers such as RETURN-FROM and GO.  It would
;;; be used when the successor of an instruction I at some lexical
;;; depth is an instruction J at a lesser lexical depth.  The
;;; procedure at the lesser lexical depth would contain a lexical
;;; location L into which some unique object (say the result of (LIST
;;; NIL)) is placed.  This instruction would then be used with L as an
;;; input.  An UNIWIND-INSTRUCTION would be inserted into the arc from
;;; I to J.  That instruction would use L as an input.  The effect
;;; would be that before J is reached, the stack would be unwound to
;;; the state it had when the CATCH-INSTRUCTION was executed. 

(defclass catch-instruction (instruction one-successor-mixin)
  ())

(defun make-catch-instruction (input &optional (successor nil successor-p))
  (make-instance 'catch-instruction
    :inputs (list input)
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNWIND-INSTRUCTION.
;;;
;;; This instruction is used to unwind the stack.  It takes a single
;;; input and it has a single successor.  It has no outputs.
;;;
;;; To implement the Common Lisp THROW special operator, it suffices
;;; to have this instruction with the value of the tag as an input and
;;; a RETURN-INSTRUCTION as its single successor.
;;;
;;; This instruction can also be used together with the
;;; CATCH-INSTRUCTION to implement lexical non-local control transfers
;;; such as RETURN-FROM and GO.  See comment for CATCH-INSTRUCTION for
;;; details.

(defclass unwind-instruction (instruction one-successor-mixin)
  ())

(defun make-unwind-instruction (input &optional (successor nil successor-p))
  (make-instance 'unwind-instruction
    :inputs (list input)
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction EQ-INSTRUCTION.

(defclass eq-instruction (instruction two-successors-mixin)
  ())

(defun make-eq-instruction (inputs successors)
  (make-instance 'eq-instruction
    :inputs inputs
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SYMBOL-VALUE-INSTRUCTION.

(defclass symbol-value-instruction (instruction one-successor-mixin)
  ())

(defun make-symbol-value-instruction (input output successor)
  (make-instance 'symbol-value-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-SYMBOL-VALUE-INSTRUCTION.

(defclass set-symbol-value-instruction (instruction one-successor-mixin)
  ())

(defun make-set-symbol-value-instruction (inputs successor)
  (make-instance 'symbol-value-instruction
    :inputs inputs
    :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FDEFINITION-INSTRUCTION.

(defclass fdefinition-instruction (instruction one-successor-mixin)
  ())

(defun make-fdefinition-instruction (input output successor)
  (make-instance 'fdefinition-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction PHI-INSTRUCTION.
;;; 
;;; This is an instruction used in SSA form.  It has at least two
;;; inputs, a single output and a single successor.
;;;
;;; Let A be some PHI-INSTRUCTION with N inputs.  A can have one or
;;; more predecessors.  If A has a single predecessor B, then B is
;;; also a PHI-INSTRUCTION with N inputs.  If A has more than one
;;; predecessor, then it has N predecessors.

(defclass phi-instruction (instruction one-successors-mixin)
  ())

(defun make-phi-instruction (inputs output successor)
  (make-instance 'eq-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))
