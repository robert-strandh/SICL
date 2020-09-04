(cl:in-package #:sicl-hir-interpreter)

(defvar *global-values-location*)

;;; The essence of this method is executed when the interpreter is
;;; about to execute an instruction that should be executed in a
;;; dynamic environment that has fewer entries than the current
;;; dynamic environment has.  In the most general case, this situation
;;; is handled by the execution of an UNWIND instruction, but no
;;; UNWIND instruction is emitted when there can be no intervening
;;; functions on the call stack.  So in this situation, we need to
;;; handle each entry in the difference between the two dynamic
;;; environments.
;;;
;;; Such an entry can be a BLOCK/TAGBODY-ENTRY, a
;;; SPECIAL-VARIABLE-ENTRY, or an UNWIND-PROTECT-ENTRY.  If it is a
;;; SPECIAL-VARIABLE-ENTRY, no action needs to be taken.  If it is an
;;; UNWIND-PROTECT-ENTRY, the corresponding thunk needs to be
;;; executed.  The tricky type is the BLOCK/TAGBODY-ENTRY.
;;;
;;; Recall that a BLOCK/TAGBODY-ENTRY is pushed onto the dynamic
;;; environment as a result of a CATCH-INSTRUCTION being interpreted.
;;; The CATCH-INSTRUCTION is interpreted as follows: We start a
;;; recursive interpreter loop interpreting successive instructions in
;;; the first successor branch of the CATCH-INSTRUCTION.  Each
;;; iteration of the loop is wrapped in a host CATCH form.  If at any
;;; point during the recursive invocation of the interpreter, an
;;; UNWIND instruction with this CATCH-INSTRUCTION as a target is
;;; encountered, the value of the host THROW is the next instruction
;;; to be executed, and it is one of the successors of the
;;; CATCH-INSTRUCTION.
;;;
;;; However, if the situation handled by this method is encountered,
;;; and we were just returning the next instruction to be executed,
;;; when a BLOCK/TAGBODY-ENTRY exists in the difference between the
;;; two environment, we would accumulate recursive invocations of the
;;; interpreter.  To avoid such accumulation, the entire recursive
;;; invocation loop is wrapped in another host CATCH form.  The value
;;; of the associated THROW is again the next instruction to be
;;; executed.
;;;
;;; So this method examines each entry in the difference between the
;;; two environments.  If a SPECIAL-VARIABLE-ENTRY is encountered, it
;;; does nothing.  If an UNWIND-PROTECT-ENTRY is encountered, it
;;; executes the thunk.  And if a BLOCK/TAGBODY-ENTRY is encountered,
;;; the last such entry is remembered.  The final action of this
;;; method is to determine whether there was such an
;;; BLOCK/TAGBODY-ENTRY, and if so, it accesses the CATCH tag to be
;;; used in a THROW form to the outer host CATCH form.  This tag is
;;; stored in the FRAME-POINTER slot of the BLOCK/TAGBODY-ENTRY.  The
;;; value thrown is the next instruction to be executed.

(defmethod interpret-instruction :before
    (client instruction lexical-environment)
  (let ((env1 (lexical-value 'dynamic-environment lexical-environment))
        (env2 (lexical-value (cleavir-ir:dynamic-environment-location
                              instruction)
                             lexical-environment)))
    (unless (or (eq env1 env2)
                (> (length env2) (length env1)))
      (loop for env = env1 then (rest env)
            for entry = (first env)
            until (eq env env2)
            do (sicl-run-time:invalidate-entry entry))
      (let ((last-block/tagbody
              (loop with result = nil
                    for env = env1 then (rest env)
                    for entry = (first env)
                    until (eq env env2)
                    when (typep entry 'sicl-run-time:unwind-protect-entry)
                      do (funcall (sicl-run-time:thunk entry))
                    when (typep entry 'sicl-run-time:block/tagbody-entry)
                      do (setf result entry)
                    finally (return result))))
        (unless (null last-block/tagbody)
          (throw (sicl-run-time:frame-pointer last-block/tagbody)
            instruction))))))

(defmethod interpret-instruction :after
    (client instruction lexical-environment)
  (declare (ignore client))
  (setf (lexical-value 'dynamic-environment lexical-environment)
        (lexical-value (cleavir-ir:dynamic-environment-location
                        instruction)
                       lexical-environment)))
