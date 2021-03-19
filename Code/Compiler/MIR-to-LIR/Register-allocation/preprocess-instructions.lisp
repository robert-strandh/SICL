(cl:in-package #:sicl-register-allocation)

(defgeneric preprocess-instruction (instruction))

;;; By default we do nothing.
(defmethod preprocess-instruction (instruction)
  nil)

(defun handle-commutative-instruction (instruction)
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
    (let ((output (first (cleavir-ir:outputs instruction))))
      (cond ((eq input1 output)
             ;; Nothing to do.  Just leave things as they are.
             nil)
            ((eq input2 output)
             ;; Swap the inputs.
             (rotatef input1 input2))
            (t
             (let ((temp (cleavir-ir:new-temporary)))
               (cleavir-ir:insert-instruction-before
                (make-instance 'cleavir-ir:assignment-instruction
                  :input input1
                  :output temp)
                instruction)
               (setf input1 temp)))))
    (reinitialize-instance instruction
      :inputs (list input1 input2))))
