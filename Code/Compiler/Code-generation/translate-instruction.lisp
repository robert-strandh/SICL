(cl:in-package #:sicl-code-generation)

(defgeneric translate-simple-instruction (instruction))

(defgeneric translate-branch-instruction (instruction next))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:nop-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:unreachable-instruction))
  '())

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:enter-instruction))
  '())

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
       (translate-datum operand)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:arithmetic-shift-right-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SAR"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

(defmethod translate-simple-instruction
    ((instruction cleavir-ir:logic-shift-right-instruction))
  (let ((destination (first (cleavir-ir:outputs instruction)))
        (operand (second (cleavir-ir:inputs instruction))))
    (make-instance 'cluster:code-command
      :mnemonic "SHR"
      :operands
      (list
       (translate-datum destination)
       (translate-datum operand)))))

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

;;; For named call instructions we generate an indirect near jump.
;;; The displacement is 0 because the address containing the jump
;;; target immediately follows the instruction.  But we set that jump
;;; target to all 0s because it is going to be filled in by the
;;; call-site manager.
(defun translate-named-call (instruction)
  ;; FIXME: The label generated as part of the code for named call
  ;; instructions needs to be associated with the IR instruction so
  ;; that it can be referred to later.
  (declare (ignore instruction))
  (list (make-instance 'cluster:code-command
          :mnemonic "JMP"
          :operands
          (list (cluster:make-memory-operand 64 :displacement 0)))
        (make-instance 'cluster:label)
        (make-instance 'cluster:data-command
          :data-bytes '(0 0 0 0 0 0 0 0))))

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
