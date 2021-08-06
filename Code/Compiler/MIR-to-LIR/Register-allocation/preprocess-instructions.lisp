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

(defmethod preprocess-instruction
    ((instruction cleavir-ir:binary-operation-mixin))
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
    ((instruction cleavir-ir:fixnum-divide-instruction))
  ;; If the divisor is a constant, introduce a lexical variable to
  ;; store it.  x86 (still) only provides encodings for dividing by a
  ;; register value or by a memory value, but not a constant.
  (destructuring-bind (dividend divisor)
      (cleavir-ir:inputs instruction)
    (when (typep divisor 'cleavir-ir:constant-input)
      (let ((lexical (cleavir-ir:make-lexical-location (gensym "DIVISOR"))))
        (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:assignment-instruction
             :input divisor
             :output lexical)
           instruction)
        (setf (cleavir-ir:inputs instruction)
              (list dividend lexical))))))
;;; However, we do not need to introduce lexical variables for
;;; constants with FIXNUM-MULTIPLY-INSTRUCTION, as RDX will always be
;;; free to store the other input.

(defmethod preprocess-instruction
    ((instruction cleavir-ir:argument-instruction))
  (destructuring-bind (index)
      (cleavir-ir:inputs instruction)
    ;; Note that the input is a boxed fixnum, so we have to scale
    ;; everything down by 2.
    (when (and (typep index 'cleavir-ir:immediate-input)
               (< (cleavir-ir:value index)
                  (* 2 (length x86-64:*argument-registers*))))
      (let ((value (floor (cleavir-ir:value index) 2)))
        (change-class instruction
          'cleavir-ir:assignment-instruction)
        (setf (cleavir-ir:inputs instruction)
              (list (nth value *bogus-argument-locations*)))))))

(defmethod preprocess-instruction
    ((instruction cleavir-ir:compute-argument-count-instruction))
  (change-class instruction
                'cleavir-ir:assignment-instruction)
  (setf (cleavir-ir:inputs instruction)
        (list *bogus-argument-count-location*)))

(defun preprocess-instructions (enter-instruction)
  (cleavir-ir:map-local-instructions
   #'preprocess-instruction
   enter-instruction))
