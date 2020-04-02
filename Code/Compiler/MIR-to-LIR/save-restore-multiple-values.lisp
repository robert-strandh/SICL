(cl:in-package #:sicl-mir-to-lir)

;;; FIXME: We must generate correct code for these instructions

(defmethod process-instruction
    ((instruction cleavir-ir:save-values-instruction)
     lexical-locations)
  (warn "Temporarily turning SAVE-VALUES into NOP.")
  (change-class instruction 'cleavir-ir:nop-instruction
                :inputs '()
                :outputs '()))

(defmethod process-instruction
    ((instruction cleavir-ir:restore-values-instruction)
     lexical-locations)
  (warn "Temporarily turning RESTORE-VALUES into NOP.")
  (change-class instruction 'cleavir-ir:nop-instruction
                :inputs '()
                :outputs '()))
