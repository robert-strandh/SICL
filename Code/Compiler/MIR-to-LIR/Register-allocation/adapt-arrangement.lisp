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
(defvar *stack-copy-register* (register-number *r15*))

(defun first-free-stack-slot (&rest arrangements)
  (reduce #'max arrangements :key #'arr:first-stack-slot-past-arrangement))

;;; Generate instructions between PREDECESSOR and SUCCESSOR which
;;; adapt the arrangement from SOURCE-ARRANGEMENT to
;;; TARGET-ARRANGEMENT.
;;;
;;; Currently we use a simple algorithm which probably generates
;;; sub-optimal code.  The basic idea is to copy every location onto
;;; the stack, past any stack slots used by either attribution, and
;;; then copy them into the registers and stack slots attributed to
;;; them in the target arrangement.  One minor catch is that copying
;;; from the stack to the stack requires a spare register; so we
;;; ensure that one register (*STACK-COPY-REGISTER*) is free by saving
;;; it first and restoring it last.
(defun adapt-arrangements-between-instructions (predecessor successor
                                                source-arrangement
                                                target-arrangement)
  (let ((next-slot
          (first-free-stack-slot source-arrangement target-arrangement))
        (scr-save-instruction nil)
        (scr-restore-instruction nil))
    ;; We maintain four "stages" of adaptation: we first save the
    ;; location stored in the stack copy register (if there is one),
    ;; then we save every other location, then we restore every other
    ;; location, then we restore the location which needs to end up in
    ;; the stack copy register (again, if there is one).
    (sicl-utilities:with-collectors ((stash   collect-stash-instruction)
                                     (restore collect-restore-instruction))
      (arr:map-attributions
       (lambda (location target-register target-stack-slot)
         (multiple-value-bind (source-register source-stack-slot)
             (arr:find-attribution source-arrangement location)
           (unless (and (eql target-register source-register)
                        (eql target-stack-slot source-stack-slot))
             (let ((temporary-stack-slot next-slot))
               (incf next-slot)
               ;; Save onto the stack.
               (cond
                 ((eql source-register *stack-copy-register*)
                  (setf scr-save-instruction
                        (save-to-stack-instruction source-register
                                                   temporary-stack-slot)))
                 ((not (null source-register))
                  (collect-stash-instruction
                   (save-to-stack-instruction source-register
                                              temporary-stack-slot)))
                 ((not (null source-stack-slot))
                  (collect-stash-instruction
                   (load-from-stack-instruction source-stack-slot
                                                *stack-copy-register*))
                  (collect-stash-instruction
                   (save-to-stack-instruction *stack-copy-register*
                                              temporary-stack-slot)))
                 (t
                  (error "~s has neither a stack nor register location in the source arrangement."
                         location)))
               ;; Load back from the stack.
               (cond
                 ((eql target-register *stack-copy-register*)
                  (setf scr-restore-instruction
                        (load-from-stack-instruction temporary-stack-slot
                                                     target-register)))
                 ((not (null target-register))
                  (collect-restore-instruction
                   (load-from-stack-instruction temporary-stack-slot
                                                target-register)))
                 ((not (null target-stack-slot))
                  (collect-restore-instruction
                   (load-from-stack-instruction temporary-stack-slot
                                                *stack-copy-register*))
                  (collect-restore-instruction
                   (save-to-stack-instruction *stack-copy-register*
                                              target-stack-slot)))
                 (t
                  (error "~s has neither a stack nor register location in the target arrangement."
                         location)))))))
       target-arrangement)
      ;; Now thread the generated instructions together.
      (let ((last-instruction predecessor))
        (flet ((insert-instruction (instruction)
                 (cleavir-ir:insert-instruction-between
                  instruction last-instruction successor)
                 (setf last-instruction instruction)))
          (unless (null scr-save-instruction)
            (insert-instruction scr-save-instruction))
          (mapc #'insert-instruction (stash))
          (mapc #'insert-instruction (restore))
          (unless (null scr-restore-instruction)
            (insert-instruction scr-restore-instruction)))))))

(defmethod introduce-registers-for-instruction ((instruction adapt-instruction))
  (destructuring-bind (successor)
      (cleavir-ir:successors instruction)
    (adapt-arrangements-between-instructions instruction successor
                                             (input-arrangement instruction)
                                             (output-arrangement instruction))
    (cleavir-ir:delete-instruction instruction)))
