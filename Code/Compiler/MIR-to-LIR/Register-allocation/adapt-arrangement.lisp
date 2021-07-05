(in-package #:sicl-register-allocation)

;;; An instruction which is a placeholder for replacement later into
;;; MIR-to-LIR.  A later pass after registers are introduced will
;;; replace an ADAPT-INSTRUCTION with instructions to adapt between
;;; arrangements.
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

;;; Generate instructions between PREDECESSOR and SUCCESSOR which
;;; adapt the arrangement from SOURCE-ARRANGEMENT to
;;; TARGET-ARRANGEMENT.
(defun adapt-arrangements-between-instructions (predecessor successor
                                                source-arrangement
                                                target-arrangement)
  (let ((next-location
          (1+ (last-used-stack-slot initial-arrangement target-arrangement)))
        (scr-save-instruction nil)
        (scr-restore-instruction nil))
    (sicl-utilities:with-collectors ((stash   collect-stash-instruction)
                                     (restore collect-restore-instruction))
      (arr:map-attributions
       (lambda (location target-register target-stack-slot)
         (multiple-value-bind (source-register source-stack-slot)
             (arr:find-attribution source-arrangement)
           (unless (and (eql target-register source-register)
                        (eql target-stack-slot source-stack-slot))
             (let ((location next-location))
               (incf next-location)
               ;; Save onto the stack.
               (cond
                 ((eql source-register *stack-copy-register*)
                  (setf scr-save-instruction
                        (save-to-stack-instruction source-register location)))
                 ((not (null source-register))
                  (collect-stash-instruction
                   (save-to-stack-instruction source-register location)))
                 ((not (null source-stack-slot))
                  (collect-stash-instruction
                   (emit-load-from-stack *stack-copy-register* source-stack-slot))
                  (collect-stash-instruction
                   (emit-save-to-stack *stack-copy-register* location)))
                 (t
                  (error "How am I supposed to save ~s?" location)))
               ;; Load back from the stack.
               (cond
                 ((eql target-register *stack-copy-register*)
                  (setf scr-restore-instruction
                        (load-from-stack-instruction target-register location)))
                 ((not (null target-register))
                  (collect-restore-instruction
                   (load-from-stack-instruction target-register location)))
                 ((not (null source-stack-slot))
                  (collect-restore-instruction
                   (load-from-stack-instruction *stack-copy-register* location))
                  (collect-restore-instruction
                   (save-to-stack-instruction *stack-copy-register* target-stack-slot)))
                 (t
                  (error "How am I supposed to recover ~s?" location)))))))
       target-arrangement)
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
