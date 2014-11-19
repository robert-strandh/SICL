(in-package #:cleavir-ir)

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

(defun make-enter-instruction
    (lambda-list &optional (successor nil successor-p))
  (let* ((outputs (loop for item in lambda-list
			append (cond ((member item lambda-list-keywords) '())
				     ((consp item) item)
				     (t (list item))))))
    (make-instance 'enter-instruction
      :lambda-list lambda-list
      :outputs outputs
      :successors (if successor-p (list successor) '()))))

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

(defun make-funcall-instruction
    (inputs outputs &optional (successor nil successor-p))
  (make-instance 'funcall-instruction
    :inputs inputs
    :outputs outputs
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
;;; This instruction is used to indicate a lexical non-local transfer
;;; of control resulting form a GO or a RETURN-FROM form.

(defclass unwind-instruction (instruction one-successor-mixin)
  (;; The invocation of the UNWIND-INSTRUCTION is the
   ;; ENTER-INSTRUCTION that represents the function invocation at
   ;; which execution should continue after the stack has been
   ;; unwound.
   (%invocation :initarg :invocation :reader invocation)))

(defun make-unwind-instruction (successor invocation)
  (make-instance 'unwind-instruction
    :inputs '()
    :outputs '()
    :successors (list successor)
    :invocation invocation))

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

(defun make-fdefinition-instruction
    (input output &optional (successor nil successor-p))
  (make-instance 'fdefinition-instruction
    :inputs (list input)
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

(defclass phi-instruction (instruction one-successor-mixin)
  ())

(defun make-phi-instruction (inputs output successor)
  (make-instance 'eq-instruction
    :inputs inputs
    :outputs (list output)
    :successors (list successor)))
