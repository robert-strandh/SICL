(cl:in-package #:sicl-hir-to-mir)

(defgeneric process-instruction (client instruction))

(defparameter *unprocessed-instruction-types*
  '(or
    cleavir-ir:enter-instruction
    cleavir-ir:nop-instruction
    cleavir-ir:assignment-instruction
    sicl-ir:breakpoint-instruction
    cleavir-ir:funcall-instruction
    cleavir-ir:return-instruction
    cleavir-ir:argument-instruction
    cleavir-ir:compute-argument-count-instruction
    cleavir-ir:unreachable-instruction
    cleavir-ir:catch-instruction
    cleavir-ir:dynamic-catch-instruction
    cleavir-ir:unwind-instruction
    sicl-ir:patch-literal-instruction
    cleavir-ir:eq-instruction
    cleavir-ir:multiple-value-call-instruction
    cleavir-ir:save-values-instruction
    cleavir-ir:restore-values-instruction
    cleavir-ir:initialize-values-instruction
    cleavir-ir:append-values-instruction
    cleavir-ir:compute-return-value-count-instruction
    cleavir-ir:return-value-instruction
    cleavir-ir:set-return-value-instruction
    cleavir-ir:initialize-return-values-instruction
    cleavir-ir:signed-add-instruction
    cleavir-ir:signed-sub-instruction
    cleavir-ir:unsigned-add-instruction
    cleavir-ir:unsigned-sub-instruction
    cleavir-ir:signed-less-instruction
    cleavir-ir:signed-not-greater-instruction
    cleavir-ir:unsigned-less-instruction
    cleavir-ir:unsigned-not-greater-instruction
    cleavir-ir:negate-instruction
    cleavir-ir:fixnump-instruction
    cleavir-ir:consp-instruction
    cleavir-ir:load-literal-instruction
    sicl-ir:named-call-instruction
    cleavir-ir:enclose-instruction
    cleavir-ir:initialize-closure-instruction
    cleavir-ir:unwind-protect-instruction
    ;; FIXME: these instructions should be processed
    cleavir-ir:standard-object-p-instruction
    cleavir-ir:single-float-p-instruction
    cleavir-ir:characterp-instruction
    cleavir-ir:standard-object-class-of-instruction
    cleavir-ir:fixnum-multiply-instruction
    cleavir-ir:fixnum-divide-instruction))

(defmethod process-instruction (client instruction)
  (declare (ignore client))
  (unless (typep instruction *unprocessed-instruction-types*)
    (error "Don't know how to handle instruction ~s" instruction)))

(defmethod process-instruction :before (client instruction)
  (declare (ignore client))
  (let ((successors (cleavir-ir:successors instruction)))
    (when (and (= (length successors) 2)
               (eq (first successors) (second successors)))
      (setf (cleavir-ir:successors instruction)
            (list (first successors)))
      (setf (cleavir-ir:predecessors (first successors))
            (remove-duplicates (cleavir-ir:predecessors (first successors))))))
  (loop for input in (cleavir-ir:inputs instruction)
        do (when (typep input 'cleavir-ir:constant-input)
             (let ((value (cleavir-ir:value input)))
               (change-class input 'cleavir-ir:immediate-input
                             :value (etypecase value
                                      (fixnum
                                       (ash value 1))
                                      (character
                                       (+ (ash (char-code value) 5) 3))))))))
