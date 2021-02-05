(cl:in-package #:sicl-hir-to-mir)

;;; We want to handle the CATCH-INSTRUCTION in a way similar to the
;;; way we handle the NAMED-CALL-INSTRUCTION.  But in HIR, the HIR
;;; evaluator makes it hard to do so, because the CATCH-INSTRUCTION
;;; has to do weird things with CL:CATCH.  In HIR, we were able to
;;; establish the CATCH-INSTRUCTION as a call site, calling the
;;; function SICL-RUN-TIME:AUGMENT-WITH-BLOCK/TAGBODY-ENTRY.
;;;
;;; The function SICL-RUN-TIME:AUGMENT-WITH-BLOCK/TAGBODY-ENTRY
;;; returns two values, corresponding to the outputs of the
;;; CATCH-INSTRUCTION, i.e. a continuation and an augmented dynamic
;;; environment.
;;;
;;; However, we were not able to add RETURN-VALUE-INSTRUCTIONs
;;; following the CATCH-INSTRUCTION, simply because the HIR
;;; evaluator, when executing the CATCH-INSTRUCTION, needs the
;;; continuation to be available, so it would be too late go create it
;;; as output of a RETURN-VALUE-INSTRUCTION.  It is convenient if
;;; those RETURN-VALUE-INSTRUCTIONs exist when we turn MIR into LIR.
;;; For that reason, we add them here, as part of the HIR-to-MIR
;;; translation phase.
;;;
;;; So we remove the outputs of the CATCH-INSTRUCTION and attach them
;;; to the RETURN-VALUE-INSTRUCTIONs instead, and we insert the
;;; RETURN-VALUE-INSTRUCTIONs in the first successor of the
;;; CATCH-INSTRUCTION.  But we do not turn the CATCH-INSTRUCTION into
;;; a FUNCALL-INSTRUCTION, because we still want to keep the
;;; additional successors of the CATCH-INSTRUCTION.

(defmethod process-instruction
    (client (instruction cleavir-ir:catch-instruction))
  (destructuring-bind (continuation-output dynamic-environment-output)
      (cleavir-ir:outputs instruction)
    (setf (cleavir-ir:outputs instruction) '())
    (cleavir-ir:insert-instruction-between
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value 2)
       :output dynamic-environment-output)
     instruction
     (first (cleavir-ir:successors instruction)))
    (cleavir-ir:insert-instruction-between
     (make-instance 'cleavir-ir:return-value-instruction
       :input (make-instance 'cleavir-ir:immediate-input
                :value 0)
       :output continuation-output)
     instruction
     (first (cleavir-ir:successors instruction)))))
