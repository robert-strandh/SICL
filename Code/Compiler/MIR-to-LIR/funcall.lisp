(cl:in-package #:sicl-mir-to-lir)

(defun load-lexical (lexical-location into-register instruction lexical-locations)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:assignment-instruction
     :input *rbp*
     :output into-register)
   instruction)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-sub-instruction
     :minuend into-register
     :subtrahend (make-instance 'cleavir-ir:immediate-input
                   :value (* 8 (1+ (gethash lexical-location lexical-locations))))
     :output into-register)
   instruction)
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:memref1-instruction
     :address into-register
     :output into-register)
   instruction))

(defun do-arguments (instruction lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction)))
    (when (> (- (length inputs) 3) 5)
      (loop for argument in (reverse (subseq inputs (+ 3 5)))
            for constant-8 = (make-instance 'cleavir-ir:immediate-input :value 8)
            do (cleavir-ir:insert-instruction-before
                (make-instance 'cleavir-ir:unsigned-sub-instruction
                  :inputs (list *rsp* constant-8)
                  :output *rsp*)
                instruction)
               (when (lexical-p argument)
                 (load-lexical argument *r11* instruction lexical-locations))
               (cleavir-ir:insert-instruction-before
                (make-instance 'cleavir-ir:memset1-instruction
                  :address *rsp*
                  :value (if (lexical-p argument)
                             *r11*
                             argument))
                instruction)))
    (loop for argument in (subseq inputs 3 (min (+ 3 5) (length inputs)))
          for register in (list *rdi* *rsi* *rdx* *rcx* *r8*)
          do (if (lexical-p argument)
                 (load-lexical argument register instruction lexical-locations)
                 (cleavir-ir:insert-instruction-before
                  (make-instance 'cleavir-ir:assignment-instruction
                    :input argument
                    :output register)
                  instruction)))))

(defun process-funcall (instruction lexical-locations)
  (do-arguments instruction lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction)))
     ;; Store the static environment to R10.
    (if (lexical-p (second inputs))
        (insert-memref-before
         instruction
         (second inputs)
         *r10*
         lexical-locations)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:assignment-instruction
           :input (second inputs)
           :output *r10*)
         instruction))
    ;; Store the dynamic environment to RBX.
    (if (lexical-p (third inputs))
        (insert-memref-before
         instruction
         (third inputs)
         *rbx*
         lexical-locations)
        (cleavir-ir:insert-instruction-before
         (make-instance 'cleavir-ir:assignment-instruction
           :input (third inputs)
           :output *rbx*)
         instruction))
    ;; Store the entry point in RAX
    (insert-memref-before
     instruction
     (first inputs)
     *rax*
     lexical-locations)
    ;; Make RAX the only input.
    (setf (cleavir-ir:inputs instruction)
          (list *rax*))))

(defmethod process-instruction
    ((instruction cleavir-ir:funcall-instruction)
     lexical-locations)
  (process-funcall instruction lexical-locations))
