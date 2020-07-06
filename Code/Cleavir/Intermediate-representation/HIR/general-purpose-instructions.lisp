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

(defun make-top-level-enter-instruction (lambda-list forms dynenv &key origin)
  (let ((enter (make-enter-instruction lambda-list dynenv :origin origin)))
    (change-class enter 'top-level-enter-instruction
		  :forms forms)))

(defmethod clone-initargs append ((instruction top-level-enter-instruction))
  (list :forms (forms instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction NOP-INSTRUCTION.

(defclass nop-instruction (one-successor-mixin instruction)
  ())

(defun make-nop-instruction (successors)
  (make-instance 'nop-instruction
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction UNREACHABLE-INSTRUCTION.

(defclass unreachable-instruction (no-successors-mixin instruction)
  ())

(defun make-unreachable-instruction ()
  (make-instance 'unreachable-instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction ASSIGNMENT-INSTRUCTION.

(defclass assignment-instruction (one-successor-mixin instruction)
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

(defclass funcall-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ((%inline :initarg :inline :initform nil :reader inline-declaration)))

(defun make-funcall-instruction
    (inputs outputs &optional (successor nil successor-p) inline)
  (make-instance 'funcall-instruction
    :inputs inputs
    :outputs outputs
    :successors (if successor-p (list successor) '())
    :inline inline))

(defmethod clone-initargs append ((instruction funcall-instruction))
  (list :inline (inline-declaration instruction)))

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
    (no-successors-mixin side-effect-mixin instruction)
  ())

(defun make-funcall-no-return-instruction (inputs)
  (make-instance 'funcall-no-return-instruction
                 :inputs inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TAILCALL-INSTRUCTION.

(defclass tailcall-instruction (no-successors-mixin instruction)
  ())

(defun make-tailcall-instruction (inputs)
  (make-instance 'tailcall-instruction
    :inputs inputs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction RETURN-INSTRUCTION.

(defclass return-instruction
    (no-successors-mixin side-effect-mixin instruction)
  ())

(defun make-return-instruction (inputs)
  (make-instance 'return-instruction
    :inputs inputs))

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

(defclass typeq-instruction (multiple-successors-mixin instruction)
  ((%value-type :initarg :value-type :reader value-type)))

(defun make-typeq-instruction (input successors value-type)
  (make-instance 'typeq-instruction
    :inputs (list input)
    :successors successors
    :value-type value-type))

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

(defclass the-instruction (one-successor-mixin instruction)
  ((%value-type :initarg :value-type :reader value-type)))

(defun make-the-instruction (input successor value-type)
  (make-instance 'the-instruction
    :inputs (list input)
    :outputs '()
    :successors (list successor)
    :value-type value-type))

(defmethod clone-initargs append ((instruction the-instruction))
  (list :value-type (value-type instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction TYPEW-INSTRUCTION.

(defclass typew-instruction (multiple-successors-mixin instruction)
  ((%ctype :initarg :ctype :reader ctype)))

(defun make-typew-instruction (input successors ctype)
  (make-instance 'typew-instruction
    :inputs (list input)
    :outputs '()
    :successors successors
    :ctype ctype))

(defmethod clone-initargs append ((instruction typew-instruction))
  (list :ctype (ctype instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CHOKE-INSTRUCTION.
;;;
;;; This is a somewhat magical instruction used in communicating type
;;; information. It indicates that type inference should treat this
;;; instruction as impassable. It has no actual semantic effect,
;;; however (i.e. is a NOP).

(defclass choke-instruction (one-successor-mixin side-effect-mixin
                             instruction)
  ())

(defun make-choke-instruction (successor)
  (make-instance 'choke-instruction
    :inputs '() :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction DYNAMIC-ALLOCATION-INSTRUCTION.
;;;
;;; This instruction has no operational effect, like THE.
;;; It informs the escape analysis machinery that values in its
;;; input will not escape the function that owns the instruction.
;;; In other words, values input to this instruction
;;; can be allocated in the local function's stack frame.

(defclass dynamic-allocation-instruction
    (one-successor-mixin instruction)
  ())

(defun make-dynamic-allocation-instruction (input successor)
  (make-instance 'dynamic-allocation-instruction
    :inputs (list input)
    :outputs nil
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CATCH-INSTRUCTION.
;;;
;;; This instruction is used to mark a control point and stack frame
;;; as an exit point. It has one input, two outputs, and one or more
;;; successors.
;;;
;;; When reached normally, control proceeds unconditionally to the
;;; first successor. It outputs a dynamic environment with a new entry
;;; added. This is the second output. The first output is a
;;; "continuation".

;;; If this continuation is used as input to an UNWIND-INSTRUCTION,
;;; the catch-instruction's stack frame is put back into place, and
;;; control proceeds to one of the catch-instruction's later
;;; successors (stored in the unwind-instruction).
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

(defclass catch-instruction (multiple-successors-mixin instruction)
  ())

(defun make-catch-instruction (continuation dynenv-out successors)
  (make-instance 'catch-instruction
    :outputs (list continuation dynenv-out)
    :successors successors))

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
;;; The instruction has two inputs: the continuation output by a
;;; CATCH-INSTRUCTION (see its comment for details) and the dynamic
;;; environment.

(defclass unwind-instruction
    (no-successors-mixin side-effect-mixin instruction)
  (;; The destination of the UNWIND-INSTRUCTION is the
   ;; instruction to which it will eventually transfer control.
   ;; This instruction must be the successor of a CATCH-INSTRUCTION.
   ;; It is not a normal successor because the exit is non-local.
   (%destination :initarg :destination :accessor destination)
   (%index :initarg :index :accessor unwind-index)))

(defun make-unwind-instruction (continuation destination index)
  (make-instance 'unwind-instruction
    :inputs (list continuation)
    :destination destination
    :index index))

(defmethod clone-initargs append ((instruction unwind-instruction))
  (list :destination (destination instruction)
        :index (unwind-index instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction LOCAL-UNWIND-INSTRUCTION.
;;;
;;; This instruction is used to indicate a lexical local transfer of
;;; control from one dynamic environment to another. This can result
;;; explicitly, from GO or RETURN-FROM, but also as the normal
;;; return from a BLOCK or TAGBODY.
;;;
;;; The process of unwinding may involve statically determined side
;;; effects due to UNWIND-PROTECT. These effects can be determined
;;; by comparing this instruction's dynamic environment with that
;;; of its successor.
;;;
;;; Unlike an UNWIND-INSTRUCTION, this instruction has an actual
;;; successor, which is probably the alternate successor of the
;;; relevant CATCH-INSTRUCTION.

(defclass local-unwind-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ())

(defun make-local-unwind-instruction (successor)
  (make-instance 'local-unwind-instruction
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction EQ-INSTRUCTION.

(defclass eq-instruction (multiple-successors-mixin instruction)
  ())

(defun make-eq-instruction (inputs successors)
  (make-instance 'eq-instruction
    :inputs inputs
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CASE-INSTRUCTION.
;;;
;;; This instruction is used to implement a multi-way comparison
;;; against sets of sets of objects, like CL:CASE.
;;;
;;; It has no outputs, and one input, the value being compared.
;;;
;;; COMPAREES is a sequence of sequences of objects, representing
;;; these sets. If COMPAREES has length n, this instruction has
;;; n+1 successors - one for each set, plus a default.

(defclass case-instruction (multiple-successors-mixin instruction)
  ((%comparees :initarg :comparees :reader comparees)))

(defun make-case-instruction (input comparees successors)
  (make-instance 'case-instruction
    :inputs (list input)
    :comparees comparees
    :successors successors))

(defmethod clone-initargs append ((instruction case-instruction))
  (list :comparees (comparees instruction)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CONSP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a CONS cell.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass consp-instruction (multiple-successors-mixin instruction)
  ())

(defun make-consp-instruction (input successors)
  (make-instance 'consp-instruction
    :inputs (list input)
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FIXNUMP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a FIXNUM.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass fixnump-instruction (multiple-successors-mixin instruction)
  ())

(defun make-fixnump-instruction (input successors)
  (make-instance 'fixnump-instruction
    :inputs (list input)
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CHARACTERP-INSTRUCTION.
;;;
;;; This instruction is used to test whether its input is a CHARACTER.
;;; If that is the case, then the first output is chosen.  Otherwise,
;;; the second output is chosen.

(defclass characterp-instruction (multiple-successors-mixin instruction)
  ())

(defun make-characterp-instruction (input successors)
  (make-instance 'characterp-instruction
    :inputs (list input)
    :successors successors))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SYMBOL-VALUE-INSTRUCTION.

(defclass symbol-value-instruction (one-successor-mixin instruction)
  ())

(defun make-symbol-value-instruction (input output successor)
  (make-instance 'symbol-value-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CONSTANT-SYMBOL-VALUE-INSTRUCTION.

(defclass constant-symbol-value-instruction (one-successor-mixin instruction)
  ((%name :initarg :name :reader name)))

(defun make-constant-symbol-value-instruction (name output successor)
  (make-instance 'constant-symbol-value-instruction
    :name name
    :inputs '()
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-SYMBOL-VALUE-INSTRUCTION.

(defclass set-symbol-value-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ())

(defun make-set-symbol-value-instruction (symbol-input value-input successor)
  (make-instance 'set-symbol-value-instruction
    :inputs (list symbol-input value-input)
    :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction SET-CONSTANT-SYMBOL-VALUE-INSTRUCTION.

(defclass set-constant-symbol-value-instruction
    (one-successor-mixin side-effect-mixin instruction)
  ((%name :initarg :name :reader name)))

(defun make-set-constant-symbol-value-instruction (name value-input successor)
  (make-instance 'set-constant-symbol-value-instruction
    :name name
    :inputs (list value-input)
    :outputs '()
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction FDEFINITION-INSTRUCTION.

(defclass fdefinition-instruction (one-successor-mixin instruction)
  ())

(defun make-fdefinition-instruction
    (input output &optional (successor nil successor-p))
  (make-instance 'fdefinition-instruction
    :inputs (list input)
    :outputs (list output)
    :successors (if successor-p (list successor) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction CONSTANT-FDEFINITION-INSTRUCTION.

(defclass constant-fdefinition-instruction (one-successor-mixin instruction)
  ((%name :initarg :name :reader name)))

(defun make-constant-fdefinition-instruction
    (name output &optional (successor nil successor-p))
  (make-instance 'constant-fdefinition-instruction
    :name name
    :inputs '()
    :outputs (list output)
    :successors (if successor-p (list successor) '())))

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

(defclass phi-instruction (one-successor-mixin instruction)
  ())

(defun make-phi-instruction (inputs output successor)
  (make-instance 'phi-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Instruction USE-INSTRUCTION.
;;;
;;; This instruction is similar to the NOP-INSTRUCTION in that it does
;;; nothing.  The difference is that this instruction takes a single
;;; input, is a lexical variable.  The purpose is to create an
;;; artificial use for the input so that it will be kept alive until
;;; after this instruction is encountered.  An instance of this
;;; instruction class will typically be emitted when the DEBUG quality
;;; has a high value and a SCOPE-AST is encountered.

(defclass use-instruction (one-successor-mixin instruction)
  ())

(defun make-use-instruction (input successor)
  (make-instance 'use-instruction
    :inputs (list input)
    :outputs '()
    :successors (list successor)))
