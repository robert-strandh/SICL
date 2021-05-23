(cl:in-package #:cleavir-partial-inlining)

;;; Cut and paste a function to inline - i.e. don't copy much of anything, which is nice,
;;; but means the original is destroyed.
(defun interpolate-function (call enter)
  (let (;; We need to alter these. We find them before doing any alteration-
        ;; interleaving modification and finds results in unfortunate effects.
        (returns '())
        (unwinds '())
        (target-enter (instruction-owner call))
        (old-dynenv (cleavir-ir:dynamic-environment-location enter))
        (call-dynenv (cleavir-ir:dynamic-environment-location call)))
    ;; Update the ownerships of each local instruction and datum and
    ;; find the exit point instructions. Also update the dynamic
    ;; environments of instructions whose dynamic environment is the
    ;; same as the one established by ENTER.
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (setf (instruction-owner instruction) target-enter)
       (when (eq (cleavir-ir:dynamic-environment-location instruction)
                 old-dynenv)
         (setf (cleavir-ir:dynamic-environment-location instruction)
               call-dynenv))
       (dolist (input (cleavir-ir:inputs instruction))
         (when (eq (location-owner input) enter)
           (setf (location-owner input) target-enter)))
       (dolist (output (cleavir-ir:outputs instruction))
         (when (eq (location-owner output) enter)
           (setf (location-owner output) target-enter)))
       (when (typep instruction 'cleavir-ir:return-instruction)
         (push instruction returns))
       (when (typep instruction 'cleavir-ir:unwind-instruction)
         (push instruction unwinds))
       (when (typep instruction 'cleavir-ir:funcall-instruction)
         (push instruction *destinies-worklist*)))
     enter)
    ;; Make appropriate assignments to do the ENTER's task.
    (loop for location in (cleavir-ir:parameters enter)
          for arg in (rest (cleavir-ir:inputs call))
          for assign = (make-instance 'binding-assignment-instruction
                                      :inputs (list arg)
                                      :outputs (list location)
                                      :dynamic-environment-location call-dynenv)
          do (push assign *binding-assignments*)
             (cleavir-ir:insert-instruction-before assign call))
    ;; Turn any unwinds in the body to the function being inlined
    ;; into direct control transfers.
    (loop with target-enter = (instruction-owner call)
          for unwind in unwinds
          for destination = (cleavir-ir:destination unwind)
          ;; Recapitulates local-catch-p in inline-one-instruction.lisp, a bit.
          when (eq (instruction-owner destination) target-enter)
            ;; it's local: replace it. (If not local, there is nothing to do.)
            ;; (Similar to the unwind-instruction method on inline-one-instruction)
            do (let* ((target (nth (cleavir-ir:unwind-index unwind)
                                   (cleavir-ir:successors destination)))
                      (new (make-instance 'cleavir-ir:nop-instruction
                             :successor target
                             :dynamic-environment-location (cleavir-ir:dynamic-environment-location unwind))))
                 (cleavir-ir:bypass-instruction new unwind)))
    ;; Replace return instructions with NOPs that go to after the call.
    (loop with next = (cleavir-ir:first-successor call)
          for return in returns
          do (let ((nop (make-instance 'cleavir-ir:nop-instruction
                          :successor next
                          :dynamic-environment-location (cleavir-ir:dynamic-environment-location return))))
               (cleavir-ir:bypass-instruction nop return))))
  ;; Replace the call with a regular control arc into the function.
  (cleavir-ir:bypass-instruction (cleavir-ir:first-successor enter) call)
  ;; Done!
  (values))

;;; Remvoe an enter instruction from the list of predecessors of its successors.
(defun disconnect-predecessor (instruction)
  (dolist (successor (cleavir-ir:successors instruction))
    (setf (cleavir-ir:predecessors successor)
          (delete instruction (cleavir-ir:predecessors successor)))))

(defun attach-predecessor (instruction)
  (dolist (successor (cleavir-ir:successors instruction))
    (push instruction (cleavir-ir:predecessors successor))))

(defmethod inline-function (initial call enter mapping)
  (let* ((*original-enter-instruction* enter)
         (*instruction-mapping* (make-hash-table :test #'eq))
         ;; Used for catch/unwind (local-catch-p)
         (*target-enter-instruction* (instruction-owner call))
         (initial-environment (cleavir-ir:parameters enter))
         (call-arguments
           (loop with dynamic-environment-location 
                   = (cleavir-ir:dynamic-environment-location call)
                 for location in initial-environment
                 for arg in (rest (cleavir-ir:inputs call))
                 for temp = (cleavir-ir:new-temporary)
                 for assign = (make-instance 'cleavir-ir:assignment-instruction
                                :input arg
                                :output temp
                                :dynamic-environment-location dynamic-environment-location)
                 do (when (cleavir-ir:using-instructions location)
                      (let ((binding-assign (first (cleavir-ir:using-instructions location))))
                        ;; Don't have to push these onto the binding
                        ;; assignment list, because only the clones
                        ;; will end up needing cells.
                        (change-class binding-assign 'binding-assignment-instruction)))
                    (cleavir-ir:insert-instruction-before assign call)
                    (setf (instruction-owner assign) *target-enter-instruction*)
                    (add-to-mapping mapping location temp)
                    (setf (location-owner temp) *target-enter-instruction*)
                 collect temp))
         (dynenv (cleavir-ir:dynamic-environment-location call))
         (function-temp (cleavir-ir:new-temporary))
         ;; This is used by the "partial" enter, but not actually connected.
         (fake-dynenv (cleavir-ir:new-temporary))
         (new-enter (cleavir-ir:clone-instruction enter
                      :dynamic-environment-location fake-dynenv))
         (enc (make-instance 'cleavir-ir:enclose-instruction
                :output function-temp
                :successor call
                :code new-enter
                :dynamic-environment-location dynenv)))
    ;; Map the old inner dynenv to the outer dynenv.
    (add-to-mapping mapping
                    (cleavir-ir:dynamic-environment-location enter)
                    (cleavir-ir:dynamic-environment-location call))
    (setf (cleavir-ir:lambda-list new-enter) '()
          ;; the temporary is the closure variable.
          (cleavir-ir:outputs new-enter) (list (cleavir-ir:new-temporary) fake-dynenv))
    ;; Ensure that the enc's successor doens't contain enc as a
    ;; predecessor, since this is outdated information.
    (disconnect-predecessor enc)
    (cleavir-ir:insert-instruction-before enc call)
    (setf (cleavir-ir:inputs call) (cons function-temp call-arguments))
    ;; If we're fully inlining a function, we want to use the call instruction's
    ;; output instead of the callee's return values.
    ;; FIXME: Not sure what to do if we're not fully inlining.
    (loop with caller-values = (first (cleavir-ir:outputs call))
          for return in (cleavir-ir:local-instructions-of-type
                         enter 'cleavir-ir:return-instruction)
          for input = (first (cleavir-ir:inputs return))
          do (add-to-mapping mapping input caller-values))
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
                      (successor (cleavir-ir:first-successor enter)))
                 (setf worklist
                       (append (inline-one-instruction
                                (enclose-instruction item)
                                (call-instruction item)
                                enter
                                successor
                                (mapping item))
                               worklist)))))))
