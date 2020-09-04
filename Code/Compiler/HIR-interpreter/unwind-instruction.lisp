(cl:in-package #:sicl-hir-interpreter)

(defmethod interpret-instruction
    (client
     (instruction cleavir-ir:unwind-instruction)
     lexical-environment)
  (destructuring-bind (function-input continuation-input)
      (cleavir-ir:inputs instruction)
    (declare (ignore function-input))
    (let* ((input-value (input-value continuation-input lexical-environment))
           (unwind-index (cleavir-ir:unwind-index instruction))
           (destination (cleavir-ir:destination instruction))
           (successors (cleavir-ir:successors destination))
           (successor (nth unwind-index successors)))
      ;; FIXME: We should search the dynamic environment for a valid
      ;; entry of type BLOCK/TAGBODY-ENTRY with an IDENTIFIER being
      ;; the same as the INPUT-VALUE, and we should execute the thunk
      ;; of any UNWIND-PROTECT entries in between.  As it is, validity
      ;; will not be checked, and no UNWIND-PROTECT entry will be
      ;; taken into account.
      (throw input-value successor))))
