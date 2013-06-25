(in-package #:sicl-mir)

;;;; MIR stands for Medium-level Intermediate Representation.  In this
;;;; representation, an AST representing a top-level form is compiled
;;;; into FLOWCHART.  A flowchart is a graph in which the nodes are
;;;; INSTRUCTIONS and LOCATIONS.  There are two types of arcs: CONTROL
;;;; ARCS and DATA ACRS.  
;;;;
;;;; A CONTROL ARC represents the flow of control in the flowchart,
;;;; and connects one instruction to another instruction.  If there is
;;;; a control arc from an instruction I to an instruction J in a
;;;; flowchart, then J is said to be A SUCCESSOR of I, and I is said
;;;; to be A PREDECESSOR of J.  An instruction can have zero, one, or
;;;; several successors.  Most instructions have a single successor.
;;;; Some instruction types such as RETURN instructions have no
;;;; successors.  Instruction types with two or more successors
;;;; represent some kind of test that can have more than one outcome.
;;;; An instruction can have zero, one, or several predecessors.  If
;;;; an instruction has no predecessors, then it is an INITIAL
;;;; INSTRUCTION of some subgraph of the flowchart, and then it can
;;;; only be reached by CALLING it, as opposed to by the ordinary flow
;;;; of control.  The entire flowchart is represented by such an
;;;; initial instruction.
;;;;
;;;; A DATA ARC represents the input to or the output from an
;;;; instruction.  A data arc with an instruction as its HEAD is an
;;;; INPUT ARC.  A data arc with an instruction as its TAIL is an
;;;; OUTPUT ARC.  The HEAD of an output arc is always a LOCATION that
;;;; can be written to.  The TAIL of an input arc is usually a
;;;; LOCATION, except that the input to an ENCLOSE instruction is the
;;;; INITIAL INSTRUCTION of some subgraph.  The output of that same
;;;; ENCLOSE instruction is a CLOSURE that, when called, transfers
;;;; control to the initial instruction that is the input of the
;;;; enclose instruction.
;;;;
;;;; An instruction J is said to be REACHABLE from some instruction I
;;;; if and only if there is a (possibly empty) sequence of control
;;;; arcs that corresponds to a path from I to J.
;;;;
;;;; The initial instructions of a flowchart form a TREE, called the
;;;; NESTING TREE.  The root of the tree is the initial instruction of
;;;; the flowchart, and the parent in the tree of some initial
;;;; instruction I other than the initial instruction of the flowchart
;;;; is the unique initial instruction from which I can be reached.
;;;; This tree is a mirror of the tree of functions in the source
;;;; code; functions defined by LAMBDA, FLET, or LABELS.
;;;;
;;;; The NESTING depth of initial instruction I is the the depth of I
;;;; in the nesting tree.
;;;;
;;;; A PROCEDURE P corresponding to some initial instruction I is a
;;;; set of instructions that are reachable from I, but that are not
;;;; reachable from any parent of I in the nesting tree.  The
;;;; instruction I is called the INITIAL INSTRUCTION OF P.
;;;;
;;;; By extension, the nesting depth of some procedure P is the
;;;; nesting depth of the initial instruction of P, and the nesting
;;;; depth of an INSTRUCTION I is the nesting depth of the procedure
;;;; of which I is a member.  Furthermore, by extension we use the
;;;; terms PARENT and ANCESTOR about procedures as well as initial
;;;; instructions with the obvious meaning.
;;;;
;;;; Clearly, given the definitions above, for every control arc A,
;;;; the HEAD of A can not have a greater nesting depth than the TAIL
;;;; of A.  However, there are control arcs for which the nesting
;;;; depth of the head is smaller than the nesting dept of the tail.
;;;; Such control arcs are the result of RETURN-FROM and GO special
;;;; operators.
;;;;
;;;; A procedure P is said to be CONTROL DEPENDENT on some procedure Q
;;;; if Q is an ancestor of P in the nesting tree and there is a
;;;; control arc from some instruction belonging to P to some
;;;; instruction belonging to Q.  A procedure P is said to be CONTROL
;;;; INDEPENDENT if and only if it is control dependent on no other
;;;; procedure.  Each control independent procedures will be turned
;;;; into a code object in itself, whereas other procedures will share
;;;; code with the procedures they depend on.  Notice that it is
;;;; possible for a procedure P to be control dependent, but not on
;;;; its direct parent.  The parent of P can thus be control
;;;; independent, whereas P will share code with some ancestor of its
;;;; parent.
;;;;
;;;; Locations are used as inputs to and outputs from instructions. 
;;;;
;;;; The possible types of locations that can be found in a MIR
;;;; program depend on the stage of translation.  Globally speaking,
;;;; there are three kinds of locations.  The first kind represents
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
;;;; nested inside Q nor is Q nested inside P, then the location is
;;;; also referred to by some instruction belonging to a procedure C
;;;; inside which both A and B are nested.
;;;;
;;;; A lexical location L is said to be PRESENT in a procedure P if and
;;;; only if some instruction belonging to P refers to L.  A location L
;;;; is said to BELONG to a procedure P if L is present in P, and L is
;;;; not present in a procedure inside which P is nested.  Because of
;;;; the restriction in the previous paragraph, every location belongs
;;;; to some unique procedure.  The procedure P to which a location
;;;; belongs is called the OWNER of the location.
;;;;
;;;; The LEXICAL DEPTH of a procedure is a quantity that is less than
;;;; or equal to the NESTING depth of that procedure.  We define it
;;;; recursively as follows: The lexical depth of a procedure P such
;;;; that every location that is present in P also belongs to P is
;;;; defined to be 0.  For a procedure A with a location present in
;;;; it, but that belongs to a different procedure Q, let D be the
;;;; greatest depth of any such procedure Q.  Then the lexical depth
;;;; of P is D+1.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a location on a stream.

(defgeneric draw-location (location stream))

;;; During the drawing process, the value of this variable is a hash
;;; table that contains locations that have already been drawn. 
(defparameter *location-table* nil)

(defmethod draw-location :around (location stream)
  (when (null (gethash location *location-table*))
    (setf (gethash location *location-table*) (gensym))
    (format stream "  ~a [shape = ellipse, style = filled];~%"
	    (gethash location *location-table*))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class IMMEDIATE-INPUT.
;;;
;;; The IMMEDIATE-INPUT location corresponds to a raw machine interger
;;; that is considered sufficiently small that it can occur directly
;;; in the instruction stream.  The machine integer is represented in
;;; the instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character. 
;;;
;;; Locations of this type are introduced by backend-specific code,
;;; because whether or not some datum can be represented as immediate
;;; input depends on the backend. 

(defclass immediate-input ()
  ((%value :initarg :value :reader value)))

(defun make-immediate-input (value)
  (make-instance 'immediate-input
    :value value))

(defmethod draw-location ((location immediate-input) stream)
  (format stream "   ~a [fillcolor = lightgreen, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (value location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class WORD-INPUT.
;;;
;;; The WORD-INPUT location corresponds to a raw machine interger that
;;; may or may not be sufficiently small to occur directly in the
;;; instruction stream.  The machine integer is represented in the
;;; instance as a Lisp integer.  The machine integer can represent
;;; some raw numeric information, or it can represent a tagged
;;; immediate Lisp datum such as a fixnum or a character.
;;;
;;; Locations of this type are introduced in early compilation stages
;;; when a WORD-AST is compiled, and in later stages when
;;; backend-specific code determins that a Lisp constant has a
;;; representation as a machine word, typically a character constant.
;;; Notice that such as character constant may or may not be possible
;;; to represent as an immediate input, depending on the Unicode code
;;; point of the character, and depending on the magnitude of
;;; immediates that the bacend can handle.

(defclass word-input ()
  ((%value :initarg :value :reader value)))

(defun make-word-input (value)
  (make-instance 'word-input
    :value value))

(defmethod draw-location ((location word-input) stream)
  (format stream "   ~a [fillcolor = lightblue, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (value location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class CONSTANT-INPUT.
;;;
;;; In later compilation stages, this location is eliminated.  If the
;;; constant can be encoded as an immediate value, then an
;;; IMMEDIATE-INPUT is used instead.  If not, then the constant is
;;; allocated in the linkage vector of the code object, and an
;;; EXTERNAL-INPUT is used instead.

(defclass constant-input ()
  ((%value :initarg :value :reader value)))

(defun make-constant-input (value)
  (make-instance 'constant-input
    :value value))

(defmethod draw-location ((location constant-input) stream)
  (format stream "   ~a [fillcolor = green, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (value location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class LEXICAL-LOCATION.
;;;
;;; In later compilation stages, this location is eliminated.  Instead
;;; an other locations are used such as STATIC-LOCATION,
;;; DYNAMIC-LOCATION, or REGISTER-LOCATION.  Not all references to a
;;; particular lexical location are replaced by the same replacement
;;; location, because where a lexical variable is to be found
;;; is typically different in different parts of the code. 

(defclass lexical-location ()
  ((%name :initarg :name :reader name)))

(defun make-lexical-location (name)
  (make-instance 'lexical-location
    :name name))

(defmethod draw-location ((location lexical-location) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (name location)))

;;; Generate a new lexical location
(defun new-temporary ()
  (make-lexical-location (gensym)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class SPECIAL-LOCATION.
;;;
;;; This location corresponds to a reference to a special variable.
;;; The storage is the place where a global value (if any) of the
;;; special variable is stored.
;;;
;;; In later compilation stages, this location is eliminated.  Instead
;;; an EXTERNAL-INPUT is used to hold the storage cell containing the
;;; global value, and this cell (together with the name) is passed as
;;; argument to a function call that searches for bindings in the
;;; dynamic environment.

(defclass special-location ()
  ((%name :initarg :name :reader name)
   (%storage :initarg :storage :reader storage)))

(defun make-special-location (name storage)
  (make-instance 'special-location
    :name name
    :storage storage))

(defmethod draw-location ((location special-location) stream)
  (format stream "   ~a [fillcolor = cyan, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (name location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class GLOBAL-INPUT.
;;;
;;; This location corresponds to a reference to a global FUNCTION,
;;; i.e., a function defined in the global environment.  
;;;
;;; In later compilation stages, this location is eliminated.  Instead
;;; an EXTERNAL-INPUT is used to hold the storage cell containing the
;;; global value, i.e. the function object that is referred to.

(defclass global-input ()
  ((%name :initarg :name :reader name)
   (%storage :initarg :storage :reader storage)))

(defun make-global-input (name storage)
  (make-instance 'global-input
    :name name
    :storage storage))

(defmethod draw-location ((location global-input) stream)
  (format stream "   ~a [fillcolor = cyan, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (name location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class EXTERNAL-INPUT.
;;;
;;; This location corresponds to an index in the linkage vector of a
;;; code object.  Later compilation stages typically replace a
;;; CONSTANT-INPUT that can not be an IMMEDIATE-INPUT, or a
;;; GLOBAL-INPUT or a by an EXTERNAL-INPUT. 

(defclass external-input ()
  ((%value :initarg :value :reader value)))

(defun make-external-input (value)
  (make-instance 'external-input
    :value value))

(defmethod draw-location ((location external-input) stream)
  (format stream "   ~a [fillcolor = pink, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (value location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class REGISTER-INPUT.
;;;
;;; This location corresponds to a processor register.  It is
;;; introduced by the register allocation phase.

(defclass register-input ()
  ((%name :initarg :name :reader name)))

(defun make-register-input (name)
  (make-instance 'register-input
    :name name))

(defmethod draw-location ((location register-input) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (name location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class STATIC-LOCATION.
;;;
;;; This location corresponds to a places in the static runtime
;;; envirionment.  That environment is organized in LAYERS, with each
;;; layer being a vector of values.  

(defclass static-location ()
  ((%layer :initarg :layer :reader layer)
   (%index :initarg :index :reader index)))

(defun make-static-location (layer index)
  (make-instance 'static-location
    :layer layer
    :index index))

(defmethod draw-location ((location static-location) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a,~a\"]~%"
	  (gethash location *location-table*)
	  (layer location)
	  (index location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location class DYNAMIC-LOCATION.
;;;
;;; This location corresponds to places in the stack frame where the
;;; compiler might decide to put objects with dynamic extent.  

(defclass dynamic-location ()
  ((%index :initarg :index :reader index)))

(defun make-dynamic-location (index)
  (make-instance 'dynamic-location
    :index index))

(defmethod draw-location ((location dynamic-location) stream)
  (format stream "   ~a [fillcolor = yellow, label = \"~a\"]~%"
	  (gethash location *location-table*)
	  (index location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions. 

(defclass instruction ()
  ((%successors :initform '() :initarg :successors :accessor successors)
   (%inputs :initform '() :initarg :inputs :reader inputs)
   (%outputs :initform '() :initarg :outputs :reader outputs)))

(defmethod initialize-instance :after ((obj instruction) &key &allow-other-keys)
  (unless (and (listp (successors obj))
	       (every (lambda (successor)
			(typep successor 'instruction))
		      (successors obj)))
    (error "successors must be a list of instructions")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing instructions.

(defparameter *instruction-table* nil)

(defgeneric draw-instruction (instruction stream))
  
(defmethod draw-instruction :around (instruction stream)
  (when (null (gethash instruction *instruction-table*))
    (setf (gethash instruction *instruction-table*) (gensym))
    (format stream "  ~a [shape = box];~%"
	    (gethash instruction *instruction-table*))
    (call-next-method)))

(defmethod draw-instruction :before ((instruction instruction) stream)
  (loop for next in (successors instruction)
	do (draw-instruction next stream))
  (loop for next in (successors instruction)
	do (format stream
		   "  ~a -> ~a [style = bold];~%"
		   (gethash instruction *instruction-table*)
		   (gethash next *instruction-table*))))
  
(defmethod draw-instruction (instruction stream)
  (format stream "   ~a [label = \"~a\"];~%"
	  (gethash instruction *instruction-table*)
	  (class-name (class-of instruction))))

(defmethod draw-instruction :after (instruction stream)
  (loop for location in (inputs instruction)
	do (draw-location location stream)
	   (format stream "  ~a -> ~a [color = red, style = dashed];~%"
		   (gethash location *location-table*)
		   (gethash instruction *instruction-table*)))
  (loop for location in (outputs instruction)
	do (draw-location location stream)
	   (format stream "  ~a -> ~a [color = blue, style = dashed];~%"
		   (gethash instruction *instruction-table*)
		   (gethash location *location-table*))))

(defun draw-flowchart (start filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede)
    (let ((*instruction-table* (make-hash-table :test #'eq))
	  (*location-table* (make-hash-table :test #'eq)))
	(format stream "digraph G {~%")
	(draw-instruction start stream)
	(format stream "}~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for Common Lisp operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOP-INSTRUCTION.

(defclass nop-instruction (instruction)
  ())

(defun make-nop-instruction (successors)
  (make-instance 'nop-instruction
    :successors successors))

(defmethod draw-instruction ((instruction nop-instruction) stream)
  (format stream "   ~a [label = \"nop\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ASSIGNMENT-INSTRUCTION.

(defclass assignment-instruction (instruction)
  ())

(defun make-assignment-instruction (input output successor)
  (make-instance 'assignment-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction
    ((instruction assignment-instruction) stream)
  (format stream "   ~a [label = \"<-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TEST-INSTRUCTION.

(defclass test-instruction (instruction)
  ())

(defun make-test-instruction (input successors)
  (make-instance 'test-instruction
    :inputs (list input)
    :successors successors))

(defmethod draw-instruction ((instruction test-instruction) stream)
  (format stream "   ~a [label = \"test\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FUNCALL-INSTRUCTION.

(defclass funcall-instruction (instruction)
  ())

(defun make-funcall-instruction (inputs successor)
  (make-instance 'funcall-instruction
    :inputs inputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction funcall-instruction) stream)
  (format stream "   ~a [label = \"funcall\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TAILCALL-INSTRUCTION.

(defclass tailcall-instruction (instruction)
  ())

(defun make-tailcall-instruction (inputs)
  (make-instance 'tailcall-instruction
    :inputs inputs))

(defmethod draw-instruction ((instruction tailcall-instruction) stream)
  (format stream "   ~a [label = \"tailcall\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction GET-VALUES-INSTRUCTION.

(defclass get-values-instruction (instruction)
  ())

(defun make-get-values-instruction (outputs successor)
  (make-instance 'get-values-instruction
    :outputs outputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction get-values-instruction) stream)
  (format stream "   ~a [label = \"get-values\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-INSTRUCTION.

(defclass return-instruction (instruction)
  ())

(defun make-return-instruction (inputs)
  (make-instance 'return-instruction
    :inputs inputs))

(defmethod draw-instruction ((instruction return-instruction) stream)
  (format stream "   ~a [label = \"ret\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ENCLOSE-INSTRUCTION.

(defclass enclose-instruction (instruction)
  ((%code :initarg :code :accessor code)))  

(defun make-enclose-instruction (output successor code)
  (make-instance 'enclose-instruction
    :outputs (list output)
    :successors (list successor)
    :code code))

(defmethod draw-instruction ((instruction enclose-instruction) stream)
  (format stream "   ~a [label = \"enclose\"];~%"
	  (gethash instruction *instruction-table*))
  (draw-instruction (code instruction) stream)
  (format stream "  ~a -> ~a [color = pink, style = dashed];~%"
	  (gethash (code instruction) *instruction-table*)
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction GET-ARGCOUNT-INSTRUCTION.

(defclass get-argcount-instruction (instruction)
  ())

(defun make-get-argcount-instruction (output successor)
  (make-instance 'get-argcount-instruction
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction get-argcount-instruction) stream)
  (format stream "   ~a [label = \"AC\", color = orange];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction GET-ARG-INSTRUCTION.

(defclass get-arg-instruction (instruction)
  ())

(defun make-get-arg-instruction (input output successor)
  (make-instance 'get-arg-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction get-arg-instruction) stream)
  (format stream "   ~a [label = \"arg\", color = orange];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for low-level operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMALLOC-INSTRUCTION.
;;;
;;; FIXME: maybe remove this one, and replace it with a function call
;;; (which could be inlined).  The corresponding function would return
;;; a fixnum which has a magnitude that is 1/4 or 1/8 of the raw
;;; address, which would make it contain exactly the same bits as the
;;; raw address itself, provided the tag for fixnums is 0.

(defclass memalloc-instruction (instruction)
  ())

(defun make-memalloc-instruction (input output successor)
  (make-instance 'memalloc-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction memalloc-instruction) stream)
  (format stream "   ~a [label = \"memalloc\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMREF-INSTRUCTION.
;;;
;;; This instruction takes a single input and a single output, and it
;;; has a single successor.  The input is a memory address and the
;;; output is the contents of that address in memory.
;;;
;;; When CACHEABLE is true, we guarantee that if the execution is
;;; executed twice with the same input, then it yields the same
;;; result.  This way, the result of the instruction can be cached in
;;; a register if the register allocator decides to do that.

(defclass memref-instruction (instruction)
  ())

(defun make-memref-instruction (input output successor &optional cacheable)
  (make-instance 'memref-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)
    :cacheable cacheable))

(defmethod draw-instruction ((instruction memref-instruction) stream)
  (format stream "   ~a [label = \"memref\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction MEMSET-INSTRUCTION.

(defclass memset-instruction (instruction)
  ())

(defun make-memset-instruction (inputs successor)
  (make-instance 'memset-instruction
    :inputs inputs
    :successors (list successor)))

(defmethod draw-instruction ((instruction memset-instruction) stream)
  (format stream "   ~a [label = \"memset\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U+-INSTRUCTION.

(defclass u+-instruction (instruction)
  ())

(defun make-u+-instruction (inputs output successors)
  (make-instance 'u+-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction u+-instruction) stream)
  (format stream "   ~a [label = \"u+\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U--INSTRUCTION.

(defclass u--instruction (instruction)
  ())

(defun make-u--instruction (inputs output successors)
  (make-instance 'u--instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction u--instruction) stream)
  (format stream "   ~a [label = \"u-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S+-INSTRUCTION.

(defclass s+-instruction (instruction)
  ())

(defun make-s+-instruction (inputs output successors)
  (make-instance 's+-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction s+-instruction) stream)
  (format stream "   ~a [label = \"s+\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S--INSTRUCTION.

(defclass s--instruction (instruction)
  ())

(defun make-s--instruction (inputs output successors)
  (make-instance 's--instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction s--instruction) stream)
  (format stream "   ~a [label = \"s-\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NEG-INSTRUCTION.

(defclass neg-instruction (instruction)
  ())

(defun make-neg-instruction (inputs output successors)
  (make-instance 'neg-instruction
    :inputs inputs
    :outputs (list output)
    :successors successors))

(defmethod draw-instruction ((instruction neg-instruction) stream)
  (format stream "   ~a [label = \"neg\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction &-INSTRUCTION.

(defclass &-instruction (instruction)
  ())

(defun make-&-instruction (inputs output successor)
  (make-instance '&-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction &-instruction) stream)
  (format stream "   ~a [label = \"&\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction IOR-INSTRUCTION.

(defclass ior-instruction (instruction)
  ())

(defun make-ior-instruction (inputs output successor)
  (make-instance 'ior-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction ior-instruction) stream)
  (format stream "   ~a [label = \"ior\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction XOR-INSTRUCTION.

(defclass xor-instruction (instruction)
  ())

(defun make-xor-instruction (inputs output successor)
  (make-instance 'xor-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction xor-instruction) stream)
  (format stream "   ~a [label = \"xor\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ~-INSTRUCTION.

(defclass ~-instruction (instruction)
  ())

(defun make-~-instruction (inputs output successor)
  (make-instance '~-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

(defmethod draw-instruction ((instruction ~-instruction) stream)
  (format stream "   ~a [label = \"~\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ==-INSTRUCTION.

(defclass ==-instruction (instruction)
  ())

(defun make-==-instruction (inputs successors)
  (make-instance '==-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction ==-instruction) stream)
  (format stream "   ~a [label = \"==\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S<-INSTRUCTION.

(defclass s<-instruction (instruction)
  ())

(defun make-s<-instruction (inputs successors)
  (make-instance 's<-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction s<-instruction) stream)
  (format stream "   ~a [label = \"s<\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction S<=-INSTRUCTION.

(defclass s<=-instruction (instruction)
  ())

(defun make-s<=-instruction (inputs successors)
  (make-instance 's<=-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction s<=-instruction) stream)
  (format stream "   ~a [label = \"s<=\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U<-INSTRUCTION.

(defclass u<-instruction (instruction)
  ())

(defun make-u<-instruction (inputs successors)
  (make-instance 'u<-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction u<-instruction) stream)
  (format stream "   ~a [label = \"u<\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction U<=-INSTRUCTION.

(defclass u<=-instruction (instruction)
  ())

(defun make-u<=-instruction (inputs successors)
  (make-instance 'u<=-instruction
    :inputs inputs
    :successors successors))

(defmethod draw-instruction ((instruction u<=-instruction) stream)
  (format stream "   ~a [label = \"u<=\"];~%"
	  (gethash instruction *instruction-table*)))

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

(defclass catch-instruction (instruction)
  ())

(defun make-catch-instruction (input successor)
  (make-instance 'catch-instruction
    :inputs (list input)
    :successors (list successor)))

(defmethod draw-instruction ((instruction catch-instruction) stream)
  (format stream "   ~a [label = \"catch\"];~%"
	  (gethash instruction *instruction-table*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNWIND-INSTRUCTION.
;;;
;;; This instruction is used to unwind the stack.  It takes a single
;;; input and it has a single successor.  It has no outputs.
;;;
;;; To implement the Common Lisp THROW special operator, it suffices
;;; have this instruction with the value of the tag as an input and a
;;; RETURN-INSTRUCTION as its single successor. 
;;;
;;; This instruction can also be used together with the
;;; CATCH-INSTRUCTION to implement lexical non-local control transfers
;;; such as RETURN-FROM and GO.  See comment for CATCH-INSTRUCTION for
;;; details.

(defclass unwind-instruction (instruction)
  ())

(defun make-unwind-instruction (input successor)
  (make-instance 'unwind-instruction
    :inputs (list input)
    :successors (list successor)))

(defmethod draw-instruction ((instruction unwind-instruction) stream)
  (format stream "   ~a [label = \"unwind\"];~%"
	  (gethash instruction *instruction-table*)))

