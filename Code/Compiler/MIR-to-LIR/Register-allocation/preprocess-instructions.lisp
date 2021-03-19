(cl:in-package #:sicl-register-allocation)

(defgeneric preprocess-instruction (instruction))

;;; By default we do nothing.
(defmethod preprocess-instruction (instruction)
  nil)

(defmethod preprocess-instruction :before
    ((instruction cleavir-ir:commutative-mixin))
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (when (typep input1 'cleavir-ir:immediate-input)
      ;; Since we are dealing with a commutative instruction, we stick
      ;; the immediate input as the second operand, so that we do not
      ;; have to create a temporary lexical location for it.
      ;;
      ;; And, make sure we have done some constant propagation to
      ;; eliminate the case where both inputs are immediate inputs.
      (assert (not (typep input2 'cleavir-ir:immediate-input)))
      (rotatef input1 input2))
    (when (eq (first (cleavir-ir:outputs instruction)) input2)
      (rotatef input1 input2))
    ;; Now, we are sure that, if an of the inputs is the same as the
    ;; output, then the first input is such an input.
    (reinitialize-instance instruction
      :inputs (list input1 input2))))

(defun handle-non-commutative-instruction (instruction)
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    (let ((output (first (cleavir-ir:outputs instruction))))
      (unless (eq input1 output)
        (let ((temp (cleavir-ir:new-temporary)))
          (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:assignment-instruction
             :input input1
             :output temp)
           instruction)
          (reinitialize-instance instruction
            :inputs (list temp input2)))))))

(defmethod preprocess-instruction
    ((instruction cleavir-ir:signed-add-instruction))
  (handle-commutative-instruction instruction))
