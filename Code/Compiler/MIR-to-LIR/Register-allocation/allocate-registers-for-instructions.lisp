(cl:in-package #:sicl-register-allocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROCESS-INPUTS.  This function may add new instructions between
;;; PREDECESSOR and INSTRUCTION in order to create a register
;;; arrangement suitable for the inputs INSTRUCTION.  It returns the
;;; last such new instruction that was added, or PREDECESSOR if no new
;;; instructions were added.

(defgeneric process-inputs (predecessor instruction))

;;; Make sure INPUT is "available".  An input is available if it is
;;; either an immediate input, or it is a lexical location, and that
;;; lexical location has a register attributed to it.
(defun ensure-input-available (predecessor instruction input)
  (if (typep input 'cleavir-ir:lexical-location)
      (ensure-lexical-location-has-attributed-register
       predecessor instruction input)
      predecessor))

;;; Make sure that every input of INSTRUCTION is available.
(defun ensure-inputs-available (predecessor instruction)
  (let ((result predecessor))
    (loop for input in (cleavir-ir:inputs instruction)
          do (setf result (ensure-input-available result instruction input)))
    result))

;;; By default, we make sure that every input that is a lexical
;;; location is in a register.
(defmethod process-inputs (predecessor instruction)
  (ensure-inputs-available predecessor instruction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROCESS-OUTPUTS.  This function may add new instructions between
;;; PREDECESSOR and INSTRUCTION in order to create an input register
;;; arrangement suitable for the outputs of INSTRUCTION.  Typically
;;; such an arrangement would have unattributed registers that can
;;; then be attributed to the outputs of INSTRUCTION in its output
;;; register arrangement.

(defgeneric process-outputs (predecessor instruction))

;;; A method on PROCESS-OUTPUTS specialized to an instruction class
;;; that has a single output that can be in any suitable register can
;;; call this function in order to implement its contract.  This
;;; function may insert new instructions between PREDECESSOR and
;;; INSTRUCTIONS in order to free up a register in CANDIDATES, so that
;;; it can then be attributed to the instruction output in the output
;;; arrangement.
(defun ensure-one-unattributed (predecessor instruction lexical-location
                                &key (disallowed-registers (make-register-map)))
  (let* ((pool (output-pool instruction))
         (candidates (register-map-difference
                      (determine-candidates lexical-location pool)
                      disallowed-registers)))
    (ensure-unattributed-registers
     predecessor instruction (output-pool predecessor) candidates 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-OUTPUT-ARRANGEMENT.

(defgeneric compute-output-arrangement (instruction arrangement))

;;; The default way of computing an output arrangement is valid when
;;; there is exactly one output, and the register attributed to it can
;;; be chosen arbitrarily.
(defun compute-output-arrangement-default (instruction arrangement)
  (let* ((output (first (cleavir-ir:outputs instruction)))
         (pool (output-pool instruction)))
    ;; We must include a new register in the arrangement.  We know
    ;; there is an unattributed register of the right kind.
    (let ((candidates (determine-candidates output pool)))
      (arr:attribute-register-for-new-lexical-location
       arrangement output candidates))
    arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SURVIVORS.  The survivors of an instruction are all those lexical
;;; locations that are live after the instruction, minus the ones that
;;; are defined by the instruction.

(defgeneric survivors (instruction))

(defmethod survivors (instruction)
  (set-difference (mapcar #'lexical-location (output-pool instruction))
                  (cleavir-ir:outputs instruction)
                  :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ALLOCATE-REGISTERS-FOR-INSTRUCTION.  This function is called for
;;; each instruction in order to compute register arrangements for
;;; that instruction.

(defgeneric allocate-registers-for-instruction (predecessor instruction))

;;; The default method starts by calling PROCESS-INPUTS to insert new
;;; predecessors according to the instruction inputs.  It then calls
;;; PROCESS-OUTPUTS with the resulting predecessors to insert more
;;; predecessors according to the instruction outputs.  Following
;;; that, it creates a skeleton arrangement from the input arrangement
;;; that has been trimmed so that only surviving lexical locations are
;;; present.  Finally, it calls COMPUTE-OUTPUT-ARRANGEMENT to finalize
;;; the output arrangement and it assigns the result as the output
;;; arrangement of the instruction.
(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let* ((new-predecessor
           (process-outputs
            (process-inputs predecessor instruction)
            instruction))
         (arrangement (arr:copy-arrangement
                       (output-arrangement new-predecessor)))
         (new-arrangement (arr:copy-arrangement arrangement)))
    (arr:trim-arrangement
     arrangement
     (mapcar #'lexical-location (input-pool instruction)))
    (setf (input-arrangement instruction) arrangement)
    (arr:trim-arrangement new-arrangement (survivors instruction))
    (setf (output-arrangement instruction)
          (compute-output-arrangement instruction new-arrangement))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Named call instructions.
;;;
;;; For call instructions, processing the inputs means making sure
;;; that every lexical location that survives the call and that has a
;;; caller-saves register attributed to it, also has a stack slot
;;; attributed to it.

(defun process-inputs-for-call-instruction (predecessor instruction)
  (let* ((survivors (survivors instruction))
         (in-caller-saves
           (arr:lexical-locations-in-register
            (output-arrangement predecessor) *caller-saves*))
         (both (intersection survivors in-caller-saves :test #'eq))
         (result predecessor))
    (loop for lexical-location in both
          do (setf result
                   (ensure-lexical-location-has-attributed-stack-slot
                    result instruction lexical-location)))
    result))

;;; For named-call instructions, there are three ways of processing
;;; outputs, depending on the exact instruction class.  If the
;;; instruction has no outputs, then nothing needs to be done.  If the
;;; instruction has one output, then we call ENSURE-ONE-UNATTRIBUTED.
;;; If the instruction has two outputs, we handle it specially.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAMED-CALL-INSTRUCTION
;;;
;;; At the moment, the NAMED-CALL-INSTRUCTION has no outputs, but that
;;; may change in the future.

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:named-call-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:named-call-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:named-call-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCALL-INSTRUCTION
;;;

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:funcall-instruction))
  ;; We need to save all the caller-saves registers, and registers
  ;; involved in the calling convention such as the argument count,
  ;; dynamic environment and static environment.  All but the dynamic
  ;; environment register are already caller-saves, so we only handle
  ;; the dynamic environment register specially.
  (let ((in-dynamic-environment
          (arr:lexical-locations-in-register
           (output-arrangement predecessor)
           (make-register-map *dynamic-environment*))))
    (loop for location in in-dynamic-environment
          do (setf predecessor
                   (ensure-lexical-location-has-attributed-stack-slot
                    predecessor instruction location))))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:funcall-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:funcall-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INITIALIZE-CLOSURE-INSTRUCTION
;;;
;;; The INITIALIZE-CLOSURE-INSTRUCTION has no outputs.

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:initialize-closure-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-closure-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:initialize-closure-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CATCH-INSTRUCTION
;;;

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:catch-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

;;; The CATCH-INSTRUCTION has two outputs, a dynamic environment and a
;;; "continuation".
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:catch-instruction))
  (destructuring-bind (continuation dynamic-environment)
      (cleavir-ir:outputs instruction)
    (let* ((pool (output-pool instruction))
           (dynamic-environment-candidates
             (determine-candidates dynamic-environment pool))
           (continuation-candidates
             (determine-candidates continuation pool)))
      (if (equal dynamic-environment-candidates continuation-candidates)
          (ensure-unattributed-registers
           predecessor
           instruction
           (output-pool predecessor)
           dynamic-environment-candidates
           2)
          (ensure-unattributed-registers
           (ensure-unattributed-registers
            predecessor
            instruction
            (output-pool predecessor)
            continuation-candidates
            1)
           instruction
           (output-pool predecessor)
           dynamic-environment-candidates
           1)))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:catch-instruction) arrangement)
  (destructuring-bind (dynamic-environment continuation)
      (cleavir-ir:outputs instruction)
    (let* ((pool (output-pool instruction))
           (dynamic-environment-candidates
             (determine-candidates dynamic-environment pool))
           (continuation-candidates
             (determine-candidates continuation pool)))
      (arr:attribute-register-for-new-lexical-location
       arrangement
       dynamic-environment
       dynamic-environment-candidates)
      (arr:attribute-register-for-new-lexical-location
       arrangement
       continuation
       continuation-candidates)
      arrangement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNWIND-PROTECT-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:unwind-protect-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:unwind-protect-instruction))
  (ensure-one-unattributed predecessor instruction
                           (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:unwind-protect-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BIND-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:bind-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

;;; The BIND-INSTRUCTION is supposed to have a single output, but
;;; HIR-to-MIR processes it by moving that output to an instruction
;;; after it. So we do not do any allocation from this instruction.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:bind-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:bind-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNWIND-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:unwind-instruction))
  (process-inputs-for-call-instruction
   (ensure-inputs-available predecessor instruction)
   instruction))

;;; The UNWIND-INSTRUCTION has no outputs.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:unwind-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:unwind-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENCLOSE-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:enclose-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

;;; The ENCLOSE-INSTRUCTION. has a single output, the resulting
;;; function object.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:enclose-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:enclose-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INITIALIZE-VALUES-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

;;; The INITIALIZE-VALUES-INSTRUCTION has a single output.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:initialize-values-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MULTIPLE-VALUE-CALL-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

;;; At the moment, the MULTIPLE-VALUE-CALL-INSTRUCTION has no outputs.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:multiple-value-call-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SAVE-VALUES-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:save-values-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:save-values-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:save-values-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RESTORE-VALUES-INSTRUCTION
;;;
;;; This instruction does not have any outputs.

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:restore-values-instruction))
  (process-inputs-for-call-instruction predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:restore-values-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:restore-values-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Comparison instructions.
;;;
;;; These instructions use the default method in PROCESS-INPUTS.

;;; These instructions do not have any outputs, so nothing needs to be
;;; done in PROCESS-OUTPUTS.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:comparison-mixin))
  predecessor)

;;; The arrangement we are given does not need to be modified in any
;;; way.
(defmethod compute-output-arrangement
    ((instruction cleavir-ir:comparison-mixin) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Test instructions.
;;;
;;; These instructions use the default method in PROCESS-INPUTS.

;;; These instructions do not have any outputs, so nothing needs to be
;;; done in PROCESS-OUTPUTS.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:test-mixin))
  predecessor)

;;; The arrangement we are given does not need to be modified in any
;;; way.
(defmethod compute-output-arrangement
    ((instruction cleavir-ir:test-mixin) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Binary operation instructions.
;;;
;;; These instructions use the default method in PROCESS-INPUTS.

;;; For a BINARY-OPERATION-MIXIN, we have prepared the instruction so
;;; that the first input is dead after the instruction.  That fact
;;; makes it possible to use the same register for the output as for
;;; the first input.  As a result, we know that no new register needs
;;; to be allocated to hold the output.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:binary-operation-mixin))
  predecessor)

;;; The x86 requires the register of the destination to be the same as
;;; the register of the first source operand.  But because of the
;;; preparation made, the first input is no longer present in
;;; ARRANGEMENT, and its register is available to attribute to the
;;; output.
(defmethod compute-output-arrangement
    ((instruction cleavir-ir:binary-operation-mixin) arrangement)
  (arr:copy-register-attribution
   (input-arrangement instruction) (first (cleavir-ir:inputs instruction))
   arrangement (first (cleavir-ir:outputs instruction)))
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIXNUM-DIVIDE-INSTRUCTION
;;;
;;; The x86 DIV instruction requires a peculiar arrangement of inputs
;;; and outputs.  The first input must be in RAX, the second input can
;;; be in any register or in memory, and RDX (the high 64 bits of the
;;; integer to divide) must be zero.  The first output will then be in
;;; RAX, and the second output will be in RDX.

;;; In an earlier version of register allocation, we planned to allow
;;; reading inputs directly from the stack, rather than calling
;;; ENSURE-INPUTS-AVAILABLE to put them in registers.  But in
;;; hindsight, I don't have a clue if it is an actually useful
;;; optimisation; if it was, it would also be applicable to any
;;; instruction which could also load from memory, but it is a hassle
;;; to implement, and I _think_ we would also want to track an
;;; estimated distance between _two_ uses in order to decide if we
;;; should use a register or not.

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:fixnum-divide-instruction))
  (ensure-inputs-available predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:fixnum-divide-instruction))
  (let ((output-pool (output-pool instruction))
        (dividend (first (cleavir-ir:inputs instruction)))
        (used-registers (make-register-map *rax* *rdx*)))
    ;; If the dividend input does not outlive this instruction, and it
    ;; is already in RAX, then we can avoid transferring it to another
    ;; register.
    (unless (and (lexical-location-in-register-p
                  (output-arrangement predecessor)
                  dividend *rax*)
                 (not (variable-live-p output-pool dividend)))
      (setf predecessor
            (ensure-register-attributions-transferred
             predecessor instruction
             output-pool
             *rax* used-registers)))
    (ensure-register-attributions-transferred
     predecessor instruction output-pool *rdx* used-registers)))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:fixnum-divide-instruction) arrangement)
  (arr:attribute-register-for-new-lexical-location
   arrangement
   (first (cleavir-ir:outputs instruction))
   (make-register-map *rax*))
  (arr:attribute-register-for-new-lexical-location
   arrangement
   (second (cleavir-ir:outputs instruction))
   (make-register-map *rdx*))
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIXNUM-MULTIPLY-INSTRUCTION
;;;
;;; x86-64 provides one-address, two-address and three-address (where
;;; one is immediate) encodings of multiplication.  However, we need
;;; to provide all 128 bits of a 64-by-64-bit multiplication, so we
;;; must use the one-address encoding.  Similar to
;;; FIXNUM-DIVIDE-INSTRUCTION, we must clear out RAX and RDX before
;;; performing a multiplication.  Unlike division, the first output
;;; (high 64 bits) will be placed in RDX, and the second (low 64 bits)
;;; will be placed in RAX.

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:fixnum-multiply-instruction))
  (ensure-inputs-available predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:fixnum-multiply-instruction))
  (let ((output-pool (output-pool instruction))
        (input1 (first (cleavir-ir:inputs instruction)))
        (used-registers (make-register-map *rax* *rdx*)))
    ;; If the first input does not outlive this instruction, and it is
    ;; already in RAX, then we can avoid transferring it to another
    ;; register.
    (unless (and (lexical-location-in-register-p
                  (output-arrangement predecessor)
                  input1 *rax*)
                 (not (variable-live-p output-pool input1)))
      (setf predecessor
            (ensure-register-attributions-transferred
             predecessor instruction
             (output-pool instruction)
             *rax* used-registers)))
    (ensure-register-attributions-transferred
     predecessor instruction
     (output-pool instruction)
     *rdx* used-registers)))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:fixnum-multiply-instruction) arrangement)
  (arr:attribute-register-for-new-lexical-location
   arrangement
   (first (cleavir-ir:outputs instruction))
   (make-register-map *rdx*))
  (arr:attribute-register-for-new-lexical-location
   arrangement
   (second (cleavir-ir:outputs instruction))
   (make-register-map *rax*))
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIGN-EXTEND-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:sign-extend-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:sign-extend-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Floating-point arithmetic instructions.

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:float-arithmetic-mixin))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:float-arithmetic-mixin) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MEMSET1-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset1-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memset1-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MEMSET2-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset2-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memset2-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MEMREF1-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memref1-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memref1-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MEMREF2-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memref2-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memref2-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ASSIGNMENT-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:assignment-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:assignment-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-ARGUMENT-COUNT-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:compute-argument-count-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:compute-argument-count-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-CONSTANT-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:load-constant-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:load-constant-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ARGUMENT-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:argument-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:argument-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INITIALIZE-RETURN-VALUES-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-return-values-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:initialize-return-values-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SET-RETURN-VALUE-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:set-return-value-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:set-return-value-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-RETURN-VALUE-COUNT-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:compute-return-value-count-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))
   :disallowed-registers *return-values*))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:compute-return-value-count-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-VALUE-INSTRUCTION

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:return-value-instruction))
  (ensure-one-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))
   :disallowed-registers *return-values*))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:return-value-instruction) arrangement)
  (compute-output-arrangement-default instruction arrangement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NOP-INSTRUCTION

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:nop-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:nop-instruction))
  predecessor)

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:nop-instruction) arrangement)
  arrangement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RETURN-INSTRUCTION.

;;; This instruction has no outputs, so nothing needs to be done in
;;; PROCESS-OUTPUTS.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:return-instruction))
  predecessor)

;;; There are no successors to this instruction, so there needs to be
;;; no output arrangement.
(defmethod compute-output-arrangement
    ((instruction cleavir-ir:return-instruction) arrangement)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNREACHABLE-INSTRUCTION.

;;; This instruction has no outputs, so nothing needs to be done in
;;; PROCESS-OUTPUTS.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:unreachable-instruction))
  predecessor)

;;; There are no successors to this instruction, so there needs to be
;;; no output arrangement.
(defmethod compute-output-arrangement
    ((instruction cleavir-ir:unreachable-instruction) arrangement)
  nil)

(defun allocate-registers-for-instructions (mir)
  (labels ((process-pair (predecessor instruction)
             (if (input-arrangement-p instruction)
                 (adapt-arrangements predecessor instruction)
                 (progn (allocate-registers-for-instruction
                         predecessor instruction)
                        (loop for successor in (cleavir-ir:successors instruction)
                              do (process-pair instruction successor))))))
    (let* ((outputs (cleavir-ir:outputs mir))
           (static-environment-location (first outputs))
           (dynamic-environment-location (second outputs)))
      (setf (output-arrangement mir)
            (make-instance 'arr:arrangement
              :register-map *initial*
              :stack-map #*
              :attributions
              (list
               (make-instance 'arr:attribution
                 :lexical-location static-environment-location
                 :stack-slot nil
                 :register-number (register-number *static-environment*))
               (make-instance 'arr:attribution
                 :lexical-location dynamic-environment-location
                 :stack-slot nil
                 :register-number (register-number *dynamic-environment*)))))
      (process-pair mir (cleavir-ir:first-successor mir)))))
