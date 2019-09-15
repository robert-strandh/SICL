(cl:in-package #:sicl-hir-to-mir)

(defgeneric process-instruction (client instruction))

(defparameter *unprocessed-instruction-types*
  '(or
    cleavir-ir:enter-instruction
    cleavir-ir:nop-instruction
    cleavir-ir:assignment-instruction
    sicl-ast-to-hir:breakpoint-instruction
    cleavir-ir:funcall-instruction
    cleavir-ir:return-instruction
    cleavir-ir:multiple-to-fixed-instruction
    cleavir-ir:fixed-to-multiple-instruction
    cleavir-ir:argument-instruction
    cleavir-ir:compute-argument-count-instruction
    cleavir-ir:unreachable-instruction
    cleavir-ir:catch-instruction
    cleavir-ir:unwind-instruction
    cleavir-ir:bind-instruction
    cleavir-ir:eq-instruction
    cleavir-ir:multiple-value-call-instruction
    cleavir-ir:save-values-instruction
    cleavir-ir:restore-values-instruction
    ;; FIXME: this instruction should be processed
    cleavir-ir:fixnum-divide-instruction))

(defmethod process-instruction (client instruction)
  (declare (ignore client))
  (unless (typep instruction *unprocessed-instruction-types*)
    (error "Don't know how to handle instruction ~s" instruction)))
