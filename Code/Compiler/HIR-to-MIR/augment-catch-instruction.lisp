(cl:in-package #:sicl-hir-to-mir)

;;; We want to handle the CATCH-INSTRUCTION in a way similar to the
;;; way we handle the FUNCALL-INSTRUCTION.  But in HIR, the HIR
;;; interpreter makes it hard to do so, because the CATCH-INSTRUCTION
;;; has to do weird things with CL:CATCH.  In HIR, we were able to add
;;; an input to the CATCH-INSTRUCTION, containing a run-time function
;;; SICL-RUN-TIME:AUGMENT-WITH-BLOCK/TAGBODY-ENTRY.  We used the
;;; FDEFINITION-INSTRUCTION to find that function, and that
;;; FDEFINITION-INSTRUCTION was then hoisted, so that the function is
;;; now fetched from the static environment.
;;;
;;; The function SICL-RUN-TIME:AUGMENT-WITH-BLOCK/TAGBODY-ENTRY
;;; returns two values, corresponding to the outputs of the
;;; CATCH-INSTRUCTION, i.e. a continuation and an augmented dynamic
;;; environment.
;;;
;;; However, we were not able to add RETURN-VALUE-INSTRUCTIONs
;;; following the CATCH-INSTRUCTION, simply because the HIR
;;; interpreter, when interpreting the CATCH-INSTRUCTION, needs the
;;; continuation to be available, so it would be too late go create it
;;; as output of a RETURN-VALUE-INSTRUCTION.  It is convenient if
;;; those RETURN-VALUE-INSTRUCTIONs exist when we turn MIR into LIR.
;;; For that reason, we add them here, as part of the HIR-to-MIR
;;; translation phase.
;;;
;;; So we remove the outputs of the CATCH-INSTRUCTION and attach them
;;; to the RETURN-VALUE-INSTRUCTIONs instead, and we insert the
;;; RETURN-VALUE-INSTRUCTIONs in the first child of the
;;; CATCH-INSTRUCTION.  But we do not turn the CATCH-INSTRUCTION into
;;; a FUNCALL-INSTRUCTION, because we still want to keep the
;;; additional children of the CATCH-INSTRUCTION.

(defmethod process-instruction
    (client (instruction cleavir-ir:catch-instruction))
  (destructuring-bind (continuation-output dynamic-environment-output)
      (cleavir-ir:outputs instruction)
    (cleavir-ir:insert-instruction-between
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:constant-input
                :value 1)
       :output dynamic-environment-output)
     instruction
     (first (cleavir-ir:successors instruction)))
    (cleavir-ir:insert-instruction-between
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:constant-input
                :value 0)
       :output continuation-output)
     instruction
     (first (cleavir-ir:successors instruction)))))
