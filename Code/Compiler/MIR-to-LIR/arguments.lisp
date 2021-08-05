(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:argument-instruction))
  ;; We have to replace the instruction with either an assignment from
  ;; the right register or stack location (if the input is an
  ;; immediate value), or an instruction sequence which picks the
  ;; right location at runtime.
  (destructuring-bind (input)
      (cleavir-ir:inputs instruction)
    (cond
      ((typep input 'cleavir-ir:immediate-input)
       (let ((value     (cleavir-ir:value input))
             (registers (length x86-64:*argument-registers*)))
         (if (< value registers)
             (change-class instruction
                           'cleavir-ir:assignment-instruction
                           :inputs (list (nth value x86-64:*argument-registers*)))
             (change-class instruction
                           'cleavir-ir:memref2-instruction
                           :inputs (list x86-64:*rsp*
                                         (* 8 (- value (1- registers))))))))
      (t
       ;; The current idea is to generate a sequence like:
       ;;   cmp Index, 0
       ;;   je read0
       ;;   cmp Index, 1
       ;;   je read1
       ;;   ...
       ;;   mov Argument, [rsp + 8 * Index]
       ;;   jmp done
       ;; read0:
       ;;   mov Argument, rdi
       ;;   jmp done
       ;; ...
       ;; done:
       (generate-ambiguous-argument-load instruction)
       (cleavir-ir:delete-instruction instruction)))))

(defvar *dynamic-environment-location*)
;; body ::= { (:just form) | (class inputs outputs) }+
(defmacro assembler (&body body)
  (destructuring-bind ((name &rest arguments) &rest rest) body
    (if (eql name :just)
        (first arguments)
        (destructuring-bind (inputs outputs)
            arguments
          `(make-instance ',name
             :inputs  (list ,@inputs)
             :outputs (list ,@outputs)
             :dynamic-environment-location *dynamic-environment-location*
             :successors (list (assembler ,@rest)))))))

(defun generate-ambiguous-argument-load (instruction)
  (let* ((successor (cleavir-ir:first-successor instruction))
         (index     (first (cleavir-ir:inputs instruction)))
         (result    (first (cleavir-ir:outputs instruction)))
         (*dynamic-environment-location*
           (cleavir-ir:dynamic-environment-location instruction))
         ;; This really does need a LEA instruction.
         (stack-code (assembler
                       (cleavir-ir:assignment-instruction (index) (result))
                       (cleavir-ir:fixnum-multiply-instruction
                        (result)
                        ((cleavir-ir:make-immediate-input 8)))
                       (cleavir-ir:fixnum-add-instruction (result) (x86-64:*rsp*))
                       (cleavir-ir:memref1-instruction (result) (result))
                       (:just successor))))
    (labels ((make-register-comparison (value registers)
               (if (null registers)
                   stack-code
                   (make-instance 'cleavir-ir:fixnum-equal-instruction
                     :inputs (list index (cleavir-ir:make-immediate-input value))
                     :dynamic-environment-location *dynamic-environment-location*
                     :successors (list
                                  (make-instance 'cleavir-ir:assignment-instruction
                                    :inputs (list (first registers))
                                    :outputs (list result)
                                    :dynamic-environment-location *dynamic-environment-location*
                                    :successors (list successor))
                                  (make-register-comparison (1+ value)
                                                            (rest registers)))))))
      (setf (cleavir-ir:successors instruction)
            (list (make-register-comparison 0 x86-64:*argument-registers*))))))
