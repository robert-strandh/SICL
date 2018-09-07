(cl:in-package #:cleavir-partial-inlining)

;;; Cut and paste a function to inline - i.e. don't copy much of anything, which is nice,
;;; but means the original is destroyed.
;;; Unused at the moment.
(defun interpolate-function (call enter)
  ;; We need to alter these. We find them before doing any alteration-
  ;; interleaving modification and finds results in unfortunate effects.
  (let ((returns (cleavir-ir:local-instructions-of-type enter 'cleavir-ir:return-instruction))
        (unwinds (cleavir-ir:local-instructions-of-type enter 'cleavir-ir:unwind-instruction)))
    ;; Make appropriate assignments to do the ENTER's task.
    (loop with cleavir-ir:*policy* = (cleavir-ir:policy call)
          for location in (rest (cleavir-ir:outputs enter))
          for arg in (rest (cleavir-ir:inputs call))
          for assign = (cleavir-ir:make-assignment-instruction arg location)
          do (cleavir-ir:insert-instruction-before assign call))
    ;; Turn any unwinds in the body to the function being inlined into
    ;; into direct control transfers.
    (loop with target-enter = (gethash call *instruction-ownerships*)
          for unwind in unwinds
          for destination = (cleavir-ir:destination unwind)
          ;; Recapitulates local-catch-p in inline-one-instruction.lisp, a bit.
          when (eq (gethash destination *instruction-ownerships*) target-enter)
            ;; it's local: replace it. (If not local, there is nothing to do.)
            ;; (Similar to the unwind-instruction method on inline-one-instruction)
            do (let* ((target (second (cleavir-ir:successors destination)))
                      (nop (let ((cleavir-ir:*policy* (cleavir-ir:policy unwind)))
                             (cleavir-ir:make-nop-instruction (list target)))))
                 (cleavir-ir:bypass-instruction nop unwind)))
    ;; Fix up the return values, and replace return instructions with NOPs that go to after the call.
    (loop with caller-values = (first (cleavir-ir:outputs call))
          with next = (first (cleavir-ir:successors call))
          for return in returns
          for values = (first (cleavir-ir:inputs return))
          do (cleavir-ir:replace-datum caller-values values)
             (let ((nop (let ((cleavir-ir:*policy* (cleavir-ir:policy return)))
                          (cleavir-ir:make-nop-instruction (list next)))))
               (cleavir-ir:bypass-instruction nop return)))
    ;; Replace the call with a regular control arc into the function.
    (cleavir-ir:bypass-instruction (first (cleavir-ir:successors enter)) call))
  ;; Done!
  (values))

(defmethod inline-function (initial call enter mapping &key uniquep)
  (let* ((*original-enter-instruction* enter)
         (*instruction-mapping* (make-hash-table :test #'eq))
         ;; Used for catch/unwind (local-catch-p)
         (*target-enter-instruction*
           (gethash call *instruction-ownerships*))
         (*copy-functions* (not uniquep))
         (*copy-locations* (not uniquep))
         (initial-environment (rest (cleavir-ir:outputs enter))) ; CAR is the closure vector - unneeded.
         ;; *policy* is bound closely for these bindings to make especially sure
         ;; that inlined instructions have the policy of the source function,
         ;; rather than the call.
         (call-arguments
           (if uniquep
               ;; We're not copying locations, even for parameters.
               ;; (We still can't use the arguments directly- might be constant, for one)
               (loop with cleavir-ir:*policy* = (cleavir-ir:policy call)
                     for location in initial-environment
                     for arg in (rest (cleavir-ir:inputs call))
                     for assign = (cleavir-ir:make-assignment-instruction arg location)
                     do (cleavir-ir:insert-instruction-before assign call)
                     finally (return initial-environment))
               ;; Usually we do make temps though.
               (loop with cleavir-ir:*policy* = (cleavir-ir:policy call)
                     for location in initial-environment
                     for arg in (rest (cleavir-ir:inputs call))
                     for temp = (cleavir-ir:new-temporary)
                     for assign = (cleavir-ir:make-assignment-instruction arg temp)
                     do (cleavir-ir:insert-instruction-before assign call)
                        (add-to-mapping mapping location temp)
                     collect temp)))
         (function-temp (cleavir-ir:new-temporary))
         (new-enter (cleavir-ir:clone-instruction enter))
         (enc (let ((cleavir-ir:*policy* (cleavir-ir:policy call)))
                (cleavir-ir:make-enclose-instruction function-temp call new-enter))))
    ;; the new ENTER shares policy and successor, but has no parameters.
    (setf (cleavir-ir:lambda-list new-enter) '()
          ;; the temporary is the closure variable.
          (cleavir-ir:outputs new-enter) (list (cleavir-ir:new-temporary)))
    (cleavir-ir:insert-instruction-before enc call)
    (setf (cleavir-ir:inputs call)
          (cons function-temp call-arguments))
    ;; If we're fully inlining a function, we want to use the call instruction's
    ;; output instead of the callee's return values.
    ;; FIXME: Not sure what to do if we're not fully inlining.
    (loop with caller-values = (first (cleavir-ir:outputs call))
          for return in (cleavir-ir:local-instructions-of-type
                         enter 'cleavir-ir:return-instruction)
          for input = (first (cleavir-ir:inputs return))
          when *copy-functions*
            do (add-to-mapping mapping input caller-values)
          else
            ;; If we're not copying functions, we need to edit them so that they refer to
            ;; the correct return values.
            ;; NOTE: Because these are inputs to return instructions, I think we can be sure
            ;; that we (and not an outer function) own them.
            do (cleavir-ir:replace-datum caller-values input))
    ;; Do the actual inlining.
    ;; FIXME: Once an inlining stops, all remaining residual functions should have
    ;; any variables live at that point added as inputs, etc.
    (let ((worklist (list (make-instance 'worklist-item
                            :enclose-instruction enc
                            :call-instruction call
                            :enter-instruction new-enter
                            :mapping mapping))))
      (loop until (null worklist)
            do (let* ((item (pop worklist))
                      (enter (enter-instruction item))
                      (successor (first (cleavir-ir:successors enter))))
                 (setf worklist
                       (append (inline-one-instruction
                                (enclose-instruction item)
                                (call-instruction item)
                                enter
                                successor
                                (mapping item))
                               worklist)))))))
