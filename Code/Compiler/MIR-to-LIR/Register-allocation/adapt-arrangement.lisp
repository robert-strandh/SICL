(in-package #:sicl-register-allocation)

;;; An instruction which is a placeholder for replacement later into
;;; MIR-to-LIR.  A later pass after registers are introduced will
;;; replace an ADAPT-INSTRUCTION with instructions to adapt between
;;; arrangements.
;;;
;;; This instruction has no inputs or outputs, and has only one
;;; predecessor and one successor.
(defclass adapt-instruction (cleavir-ir:instruction)
  ())

(defun adapt-arrangements (predecessor instruction)
  (cond
    ((arr:arrangements-compatible-p
      (output-arrangement predecessor)
      (input-arrangement instruction))
     nil)
    (t
     (let ((adapt-instruction (make-instance 'adapt-instruction)))
       (setf (input-arrangement adapt-instruction)
             (output-arrangement predecessor)
             (output-arrangement adapt-instruction)
             (input-arrangement instruction))
       (cleavir-ir:insert-instruction-between
        adapt-instruction
        predecessor instruction)))))

;;; A totally arbitrary choice of register, used temporarily to
;;; transfer values from the stack back to the stack, should there be no
;;; other free registers.
(defvar *stack-copy-register* x86-64:*r15*)

(defun first-free-stack-slot (&rest arrangements)
  (reduce #'max arrangements :key #'arr:first-stack-slot-past-arrangement))

;;; A class which just makes it easier to find instructions generated
;;; by adaptation.
(defclass adapt-assignment-instruction (cleavir-ir:assignment-instruction)
  ())

;;; Generate instructions between PREDECESSOR and SUCCESSOR which
;;; adapt the arrangement from SOURCE-ARRANGEMENT to
;;; TARGET-ARRANGEMENT.
;;;
;;; Currently we use an algorithm which generates acceptable code.
;;; The basic idea is to try to copy every location normally, but if
;;; we need to use a register we already overwrote, we spill it before
;;; any register assignments are actually done.  One minor catch is
;;; that copying from the stack to the stack requires a spare
;;; register; so we ensure that one register (*STACK-COPY-REGISTER*)
;;; is free by saving it first and restoring it last.
(defun adapt-arrangements-between-instructions (predecessor successor
                                                source-arrangement
                                                target-arrangement)
  ;; TODO: pick a register unused in either arrangement if possible.
  ;; We redirect loads from and stores to the stack copy register to a
  ;; slot on the stack, and then emit spill and unspill instructions if
  ;; we found we used the register.
  (let* ((stack-copy-register *stack-copy-register*)
         (stack-copy-number (x86-64:register-number stack-copy-register))
         (stack-copy-slot
           (first-free-stack-slot source-arrangement target-arrangement))
         (next-slot (1+ stack-copy-slot))
         (clobbered-registers (list stack-copy-number))
         (clobbered-stack-slots '())
         (loaded-stack-copy-p nil)
         (stored-stack-copy-p nil))
    (sicl-utilities:with-collectors ((spills    add-spill-instruction)
                                     (transfers add-transfer-instruction))
      (arr:map-attributions
       (lambda (location target-register target-stack-slot)
         (multiple-value-bind (source-stack-slot source-register)
             (arr:find-attribution source-arrangement location)
           (assert (not (and (null source-stack-slot)
                             (null source-register))))
           ;; Redirect loads from the stack copy register.
           (when (eql source-register stack-copy-number)
             (setf source-register     nil
                   source-stack-slot   stack-copy-slot
                   loaded-stack-copy-p t))
           (when (eql target-register stack-copy-number)
             (setf target-register     nil
                   target-stack-slot   stack-copy-slot
                   stored-stack-copy-p t))
           ;; Don't emit anything for no-ops.  Note that we still need
           ;; to track if a location is always in the stack copy
           ;; register, so the rewriting logic must be unconditional.
           (unless (and (eql source-register target-register)
                        (eql source-stack-slot target-stack-slot))
             ;; Check that we won't attempt to read from a location we
             ;; already wrote to.
             (when (or (member source-register clobbered-registers)
                       (member source-stack-slot clobbered-stack-slots))
               ;; If we have spilled, pick a fresh stack slot, then
               ;; spill to that stack slot.
               (let ((spill-slot next-slot))
                 (incf next-slot)
                 (cond
                   ((null source-stack-slot)
                    (add-spill-instruction
                     (x86-64:save-to-stack-instruction
                      (aref x86-64:*registers* source-register)
                      spill-slot)))
                   (t
                    ;; Copy from stack to stack.
                    (add-spill-instruction
                     (x86-64:load-from-stack-instruction
                      source-stack-slot stack-copy-register))
                    (add-spill-instruction
                     (x86-64:save-to-stack-instruction
                      stack-copy-register spill-slot))))
                 (setf source-stack-slot spill-slot
                       source-register   nil)))
             (cond
               ((not (null target-register))
                (add-transfer-instruction
                 (if (not (null source-register))
                     (make-instance 'adapt-assignment-instruction
                       :inputs  (list (aref x86-64:*registers* source-register))
                       :outputs (list (aref x86-64:*registers* target-register)))
                     (x86-64:load-from-stack-instruction
                      source-stack-slot
                      (aref x86-64:*registers* target-register))))
                (push target-register clobbered-registers))
               ((not (null target-stack-slot))
                (cond
                  ((not (null source-register))
                   (add-transfer-instruction
                    (x86-64:save-to-stack-instruction
                     (aref x86-64:*registers* source-register)
                     target-stack-slot)))
                  (t
                   (add-transfer-instruction
                    (x86-64:load-from-stack-instruction
                     source-stack-slot stack-copy-register))
                   (add-transfer-instruction
                    (x86-64:save-to-stack-instruction
                     stack-copy-register target-stack-slot))))
                (push target-stack-slot clobbered-stack-slots))))))
       target-arrangement)
      ;; Now thread the generated instructions together.
      (let ((last-instruction predecessor))
        (flet ((insert-instruction (instruction)
                 (mark-as-generated instruction)
                 (cleavir-ir:insert-instruction-between
                  instruction last-instruction successor)
                 (setf last-instruction instruction)))
          (when loaded-stack-copy-p
            (insert-instruction
             (x86-64:save-to-stack-instruction
              stack-copy-register stack-copy-slot)))
          (mapc #'insert-instruction (spills))
          (mapc #'insert-instruction (transfers))
          (when stored-stack-copy-p
            (insert-instruction
             (x86-64:load-from-stack-instruction
              stack-copy-slot stack-copy-register))))))))

(defmethod introduce-registers-for-instruction ((instruction adapt-instruction))
  (destructuring-bind (successor)
      (cleavir-ir:successors instruction)
    (adapt-arrangements-between-instructions instruction successor
                                             (input-arrangement instruction)
                                             (output-arrangement instruction))
    (cleavir-ir:delete-instruction instruction)))
