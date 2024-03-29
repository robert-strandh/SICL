(cl:in-package #:sicl-code-generation)

(defgeneric translate-simple-instruction (instruction))

(defgeneric translate-branch-instruction (instruction next))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction))
  '())

;;; For an ENTER-INSTRUCTION, we create a full word that will contain
;;; a reference to the function descriptor for functions represented
;;; by this ENTER-INSTRUCTION.
;;;
;;; FIXME: Cluster needs an ALIGN pseudo-instruction so that this word
;;; and the entry point of the function can be word aligned.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction))
  (make-instance 'cluster:data-command
            :data-bytes '(0 0 0 0 0 0 0 0)))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:breakpoint-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:load-literal-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum destination)
       (make-instance 'cluster:immediate-operand
         :value (sicl-memory:pointer
                 (car (cleavir-ir:location-info instruction))))))))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:load-effective-address-instruction))
  (let ((source (first (cleavir-ir:inputs instruction)))
        (destination (first (cleavir-ir:outputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "LEA"
      :operands
      (list
       (translate-datum destination)
       (translate-datum source)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:assignment-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (source (first (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "MOV"
      :operands
      (list
       (translate-datum destination)
       (translate-datum source)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:shift-left-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SHL"
      :operands
      (list
       (translate-datum destination)
       (if (typep operand 'cleavir-ir:immediate-input)
           (translate-datum operand)
           (make-instance 'cluster:gpr-operand
             :size 8
             :code-number 2))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:arithmetic-shift-right-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SAR"
      :operands
      (list
       (translate-datum destination)
       (if (typep operand 'cleavir-ir:immediate-input)
           (translate-datum operand)
           (make-instance 'cluster:gpr-operand
             :size 8
             :code-number 2))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:logic-shift-right-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SHR"
      :operands
      (list
       (translate-datum destination)
       (if (typep operand 'cleavir-ir:immediate-input)
           (translate-datum operand)
           (make-instance 'cluster:gpr-operand
             :size 8
             :code-number 2))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:return-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "RET"
    :operands '()))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:funcall-instruction))
  (make-instance 'cluster:code-command
    :mnemonic "CALL"
    :operands
    (list (translate-datum (first (cleavir-ir:inputs instruction))))))

;;; When code is generated, this variable contains an EQ hash table
;;; that maps named call instrucitons to labels so that the label
;;; associated with such an instruction is the word to be filled in by
;;; the call site manager to contain the address of the trampoline
;;; snippet associated with this call site.
(defvar *call-site-labels*)

;;; For named call instructions we generate an indirect near jump.
;;; The displacement is 0 because the address containing the jump
;;; target immediately follows the instruction.  But we set that jump
;;; target to all 0s because it is going to be filled in by the
;;; call-site manager.
(defun translate-named-call (instruction)
  (let ((label (make-instance 'cluster:label)))
    (setf (gethash instruction *call-site-labels*) label)
    (list (make-instance 'cluster:code-command
            :mnemonic "JMP"
            :operands
            (list (cluster:make-memory-operand 64 :displacement 0)))
          label
          (make-instance 'cluster:data-command
            :data-bytes '(0 0 0 0 0 0 0 0)))))

;;; FIXME: We shouldn't really have any of these.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:named-call-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:named-call-instruction))
  (translate-named-call instruction))

;;; This method is called when the CATCH-INSTRUCTION has a single
;;; successor.  The only way that I can see that happening would be
;;; if there is an empty TAGBODY.
(defmethod translate-simple-instruction
    ((instruction cleavir-ir:catch-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:dynamic-catch-instruction))
  (translate-named-call instruction))

(defmethod translate-branch-instruction
    ((instruction cleavir-ir:catch-instruction) next)
  (let ((successors (cleavir-ir:successors instruction)))
    (append (translate-named-call instruction)
            (if (eq (cleavir-ir:first-successor instruction) next)
                '()
                (list (make-instance 'cluster:code-command
                        :mnemonic "JMP"
                        :operands
                        (list (find-instruction-label (first successors)))))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:bind-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-instruction))
  (append (translate-named-call instruction)
          (make-instance 'cluster:code-command
            :mnemonic "JMP"
            :operands
            (list (find-instruction-label
                   (nth (cleavir-ir:unwind-index instruction)
                        (cleavir-ir:successors
                         (cleavir-ir:destination instruction))))))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:multiple-value-call-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:initialize-values-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:save-values-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:restore-values-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unwind-protect-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction sicl-ir:patch-literal-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enclose-instruction))
  (translate-named-call instruction))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:initialize-closure-instruction))
  (translate-named-call instruction))
