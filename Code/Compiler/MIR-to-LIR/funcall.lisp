(cl:in-package #:sicl-mir-to-lir)

(defun do-arguments (instruction)
  (let ((inputs (cleavir-ir:inputs instruction)))
    (when (> (- (length inputs) 3) 5)
      (loop for argument in (reverse (subseq inputs (+ 3 5)))
            for constant-8 = (make-instance 'cleavir-ir:immediate-input :value 8)
            do (cleavir-ir:insert-instruction-before
                (make-instance 'cleavir-ir:unsigned-sub-instruction
                  :inputs (list *rsp* constant-8)
                  :output *rsp*)
                instruction)))
    (loop for argument in (subseq inputs 3 (min (+ 3 5) (length inputs)))
          for register in (list *rdi* *rsi* *rdx* *rcx* *r8*)
          do (cleavir-ir:insert-instruction-before
              (make-instance 'cleavir-ir:assignment-instruction
                :input argument
                :output register)
              instruction))))

(defmethod process-instruction
    ((instruction cleavir-ir:funcall-instruction)
     lexical-locations)
  (do-arguments instruction)
  (let ((inputs (cleavir-ir:inputs instruction)))
     ;; Store the static environment to R10.
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input (second inputs)
       :output *r10*)
     instruction)
    ;; Store the dynamic environment to RBX.
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input (third inputs)
       :output *rbx*)
     instruction)
    ;; Leave only the entry point as input
    (setf (cleavir-ir:inputs instruction)
          (list (first inputs)))))
