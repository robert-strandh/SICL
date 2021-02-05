(cl:in-package #:cleavir-ir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instructions for Common Lisp operators.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TOP-LEVEL-ENTER-INSTRUCTION.
;;;
;;; This is a subclass of ENTER-INSTRUCTION used as the initial
;;; instruction of a flowchart that represents a LOAD FUNCTION.
;;;
;;; An initial form F to be evaluated is turned into a load function
;;; LF.  When LF is called with the values of the LOAD-TIME-VALUE
;;; forms (including non-immediate constants) of F, then the result is
;;; the values and the effects of evaluating F.
;;;
;;; The TOP-LEVEL-ENTER-INSTRUCTION supplies a slot containing the
;;; list of the LOAD-TIME-VALUE forms to be evaluated before being
;;; supplied as arguments.

(defclass top-level-enter-instruction (enter-instruction)
  ((%forms :initarg :forms :accessor forms)))

(defun make-top-level-enter-instruction (lambda-list forms dynenv)
  (let ((enter (make-enter-instruction lambda-list dynenv)))
    (change-class enter 'top-level-enter-instruction
                  :forms forms)))

(defmethod clone-initargs append ((instruction top-level-enter-instruction))
  (list :forms (forms instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOP-INSTRUCTION.

(defclass nop-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNREACHABLE-INSTRUCTION.

(defclass unreachable-instruction (instruction no-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ASSIGNMENT-INSTRUCTION.

(defclass assignment-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FUNCALL-INSTRUCTION.

(defclass funcall-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FUNCALL-NO-RETURN-INSTRUCTION.
;;;
;;; This is for calls that are known to never return normally,
;;; e.g. calls to ERROR. Having no successor simplifies analysis
;;; by making whatever leads here irrelevant to other code.
;;;
;;; It's a separate class because funcall having one-successor-mixin
;;; is pretty convenient.

(defclass funcall-no-return-instruction
    (instruction no-successors-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TAILCALL-INSTRUCTION.

(defclass tailcall-instruction (instruction no-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NAMED-CALL-INSTRUCTION.
;;;
;;; This instruction can be used by clients that want a specific
;;; representation for function calls where the callee is a global
;;; function that is explicitly named in source code.  This
;;; instruction differs from the FUNCALL-INSTRUCTION in that it has no
;;; input for the callee; only for the arguments.  Instead, the name
;;; of the callee is stored in a slot of the instruction.

(defclass named-call-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ((%callee-name :initarg :callee-name :reader callee-name)))

(defmethod clone-initargs append ((instruction named-call-instruction))
  (list :callee-name (callee-name instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-INSTRUCTION.

(defclass return-instruction
    (instruction no-successors-mixin side-effect-mixin)
  ())

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

(defclass typeq-instruction (instruction multiple-successors-mixin)
  ((%value-type :initarg :value-type :reader value-type)))

(defmethod clone-initargs append ((instruction typeq-instruction))
  (list :value-type (value-type instruction)))

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

(defmethod clone-initargs append ((instruction the-instruction))
  (list :value-type (value-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DYNAMIC-ALLOCATION-INSTRUCTION.
;;;
;;; This instruction has no operational effect, like THE.  It informs
;;; the escape analysis machinery that values in its input will not
;;; escape the function that owns the instruction.  In other words,
;;; values input to this instruction can be allocated in the stack
;;; frame of the local function.

(defclass dynamic-allocation-instruction
    (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CATCH-INSTRUCTION.
;;;
;;; This instruction is used to mark a control point and stack frame
;;; as an exit point. It has no inputs, two outputs, and one or more
;;; successors.
;;;
;;; When reached normally, control proceeds unconditionally to the
;;; first successor. It outputs a dynamic environment with a new entry
;;; added. This is the second output. The first output is a
;;; "continuation".

;;; If this continuation is used as input to an UNWIND-INSTRUCTION,
;;; the stack frame of the catch-instruction is put back into place,
;;; and control proceeds to one of the later successors of the
;;; catch-instruction.  Which successor is to be used, is indicated by
;;; the INDEX of the UNWIND-INSTRUCTION.
;;;
;;; The continuation can only be used once, and cannot be used after the
;;; function containing the CATCH has returned or been unwound from.
;;; In Scheme terms, it is a one-shot escape continuation.
;;;
;;; This instruction can be used to implement the Common Lisp BLOCK
;;; and/or TAGBODY special operators, among other possible nonlocal
;;; exit constructors. A BLOCK would be compiled into a CATCH with
;;; the block body as first successor, and return point as second.
;;; Functions returning to the BLOCK would UNWIND to the CATCH, using
;;; a closed-over continuation.

(defclass catch-instruction (instruction multiple-successors-mixin)
  ())

(defmethod dynamic-environment-output ((instruction catch-instruction))
  (second (outputs instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNWIND-INSTRUCTION.
;;;
;;; This instruction is used to indicate a lexical non-local transfer
;;; of control resulting from a GO or a RETURN-FROM form.
;;;
;;; The process of unwinding may involve dynamically determined side
;;; effects due to UNWIND-PROTECT.
;;;
;;; The instruction has a single input: the continuation output by a
;;; CATCH-INSTRUCTION (see its comment for details).
;;;
;;; This instruction has no "normal" successors.  The instruction to
;;; which control is to be transferred is instead determined by the
;;; combination of the DESTINATION and the INDEX.

(defclass unwind-instruction
    (instruction no-successors-mixin side-effect-mixin)
  (;; The destination of the UNWIND-INSTRUCTION is the
   ;; CATCH-INSTRUCTION that is the result of compiling the
   ;; corresponding BLOCK-AST.
   (%destination :initarg :destination :accessor destination)
   ;; The index of the UNWIND-INSTRUCTION is the index into the list
   ;; of successors of the CATCH-INSTRUCTION that is the DESTINATION.
   (%index :initarg :index :accessor unwind-index)))

(defmethod clone-initargs append ((instruction unwind-instruction))
  (list :destination (destination instruction)
        :index (unwind-index instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction BIND-INSTRUCTION.
;;;
;;; This instruction is used to bind a special variable to a value
;;; and then execute some code in a new run-time environment in
;;; which that binding is in force.
;;;
;;; The instruction has two inputs.  The first input is a symbol
;;; naming the variable to be bound.  The second input is the value
;;; that the variable is to be bound to.
;;;
;;; The instruction has a single output, a lexical location indicating
;;; a new dynamic environment in which the variable is bound.

(defclass bind-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defmethod dynamic-environment-output ((instruction bind-instruction))
  (first (outputs instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNWIND-PROTECT-INSTRUCTION.
;;;
;;; This instruction is used in order to create an entry on the
;;; dynamic environment that will perform the cleanup actions
;;; specified in an UNWIND-PROTECT form whenever the environment is
;;; unwound.
;;;
;;; The instruction has a single input, namely a thunk, typically
;;; resulting from the execution of an ENCLOSE-INSTRUCTION of the code
;;; in the cleanup forms of the UNWIND-PROTECT form.
;;;
;;; The instruction has a single output, a lexical location indicating
;;; a new dynamic environment in which the variable is bound.

(defclass unwind-protect-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

(defmethod dynamic-environment-output ((instruction unwind-protect-instruction))
  (first (outputs instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction EQ-INSTRUCTION.

(defclass eq-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CONSP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a CONS cell.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass consp-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUMP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a FIXNUM.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass fixnump-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SHORT-FLOAT-P-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a
;;; SHORT-FLOAT.  If that is the case, then the first output is
;;; chosen.  Otherwise, the second output is chosen.
;;;
;;; This instruction can be used by clients that represent short
;;; floats as immediate objects.

(defclass short-float-p-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SINGLE-FLOAT-P-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a
;;; SINGLE-FLOAT.  If that is the case, then the first output is
;;; chosen.  Otherwise, the second output is chosen.
;;;
;;; This instruction can be used by clients that represent single
;;; floats as immediate objects.

(defclass single-float-p-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction STANDARD-OBJECT-P-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a
;;; STANDARD-OBJECT.  If that is the case, then the first output is
;;; chosen.  Otherwise, the second output is chosen.
;;;
;;; This instruction can be used by clients that have unique tag bits
;;; for representing standard objects.

(defclass standard-object-p-instruction (instruction multiple-successors-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SYMBOL-VALUE-INSTRUCTION.
;;;
;;; This instruction is used when the value of a special variable is
;;; required.  It has a single input, a constant input containing the
;;; symbol naming the variable.  It has a single output, a lexical
;;; location that will hold the value of the special variable.

(defclass symbol-value-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-SYMBOL-VALUE-INSTRUCTION.
;;;
;;; This instruction is generated as a result of an assignment to a
;;; special variable.  It has two inputs, a constant input containing
;;; the symbol naming the variable, and a lexical location or a
;;; constant input that will become the value of the special variable.
;;; This instruction has no outputs.

(defclass set-symbol-value-instruction
    (instruction one-successor-mixin side-effect-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FDEFINITION-INSTRUCTION.

(defclass fdefinition-instruction (instruction one-successor-mixin)
  ())

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

(defclass phi-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction USE-INSTRUCTION.
;;;
;;; This instruction is similar to the NOP-INSTRUCTION in that it does
;;; nothing.  The difference is that this instruction takes a single
;;; input, which is a lexical location.  The purpose is to create an
;;; artificial use for the input so that it will be kept alive until
;;; after this instruction is encountered.  An instance of this
;;; instruction class will typically be emitted when the DEBUG quality
;;; has a high value and a SCOPE-AST is encountered.

(defclass use-instruction (instruction one-successor-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LOAD-CONSTANT-INSTRUCTION.
;;;
;;; This instruction can be used by clients who have a way of loading
;;; constants directly from the instruction stream.  It has no inputs.
;;; It has a single output which is a lexical location into which the
;;; constant will be loaded.  The instruction itself contains a slot
;;; that can be used by client code to store information about where
;;; the constant is to be found.

(defclass load-constant-instruction (instruction one-successor-mixin)
  ((%location-info :initarg :location-info :reader location-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNWIND-PROTECT-INSTRUCTION.
;;;
;;; This instruction has one input, which contains a thunk.  It has a
;;; single DYNAMIC-ENVIRONMENT-OUTPUT.  The dynamic environment of
;;; that output is the dynamic environment of the instruction itself,
;;; augmented with an UNWIND-PROTECT entry.  The protected forms of
;;; the UNWIND-PROTECT form are compiled in the augmented dynamic
;;; environment.

(defclass unwind-protct-instruction (instruction one-successor-mixin)
  ())

(defmethod dynamic-environment-output ((instruction unwind-protect-instruction))
  (first (outputs instruction)))
