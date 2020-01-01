(cl:in-package #:sicl-mir-to-lir)

(defgeneric process-instruction (instruction lexical-locations))

(defmethod process-instruction (instruction lexical-locations)
  nil)

;;; Return a list of instructions that, when executed, loads the
;;; address of LEXICAL-LOCATION into TO-REGISTER.
(defun load-address-of-lexical-location
    (lexical-location
     to-register
     lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (* (1+ (gethash lexical-location lexical-locations)) 8))))
    (list (make-instance 'cleavir-ir:assignment-instruction
            :input *rbp*
            :output to-register)
          (make-instance 'cleavir-ir:unsigned-sub-instruction
            :inputs (list to-register immediate-input)
            :output to-register))))

(defun insert-memref-before
    (instruction
     from-lexical-location
     to-register
     scratch-register
     lexical-locations)
  (let ((load-instructions
          (load-address-of-lexical-location
           from-lexical-location scratch-register lexical-locations)))
    (loop for load-instruction in load-instructions
          do (cleavir-ir:insert-instruction-before load-instruction instruction)))
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:memref1-instruction
     :input scratch-register
     :output to-register)
   instruction))

(defun insert-memset-after
    (instruction
     from-register
     to-lexical-location
     scratch-register
     lexical-locations)
  (cleavir-ir:insert-instruction-after
   (make-instance 'cleavir-ir:memset1-instruction
     :inputs (list scratch-register from-register))
   instruction)
  (let ((load-instructions
          (load-address-of-lexical-location
           to-lexical-location scratch-register lexical-locations)))
    (loop for load-instruction in (reverse load-instructions)
          do (cleavir-ir:insert-instruction-after load-instruction instruction))))

(defmethod process-instruction (instruction lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (assert (<= 1 (length inputs) 2))
    (assert (= (length outputs) 1))
    (if (typep (first inputs) 'cleavir-ir:lexical-location)
        (if (typep (first outputs) 'cleavir-ir:lexical-location)
            ;; We use *r11* as an intermediate register
            (progn (insert-memref-before
                    instruction
                    (first inputs)
                    *r11*
                    *rax*
                    lexical-locations)
                   (setf (first inputs) *r11*)
                   (insert-memset-after
                    instruction
                    *r11*
                    (first outputs)
                    *rax*
                    lexical-locations)
                   (setf (first outputs) *r11*)
                   (when (and (= (length inputs) 2)
                              (typep (second inputs) 'cleavir-ir:lexical-location))
                     (insert-memref-before
                      instruction
                      (second inputs)
                      *rax*
                      *rax*
                      lexical-locations)))
            ;; We use the output register as an intermediate register
            (progn (insert-memref-before
                    instruction
                    (first inputs)
                    (first outputs)
                    *rax*
                    lexical-locations)
                   (setf (first inputs) (first outputs))
                   (when (and (= (length inputs) 2)
                              (typep (second inputs) 'cleavir-ir:lexical-location))
                     (insert-memref-before
                      instruction
                      (second inputs)
                      *rax*
                      *rax*
                      lexical-locations))))
        (if (typep (first outputs) 'cleavir-ir:lexical-location)
            (progn (cleavir-ir:insert-instruction-before
                    (make-instance 'cleavir-ir:assignment-instruction
                      :input (first inputs)
                      :output *r11*)
                    instruction)
                   (setf (first inputs) *r11*)
                   (insert-memset-after
                    instruction
                    *r11*
                    (first outputs)
                    *rax*
                    lexical-locations)
                   (setf (first outputs) *r11*)
                   (when (and (= (length inputs) 2)
                              (typep (second inputs) 'cleavir-ir:lexical-location))
                     (insert-memref-before
                      instruction
                      (second inputs)
                      *rax*
                      *rax*
                      lexical-locations)))
            (unless (eq (first inputs) (first outputs))
              (cleavir-ir:insert-instruction-before
               (make-instance 'cleavir-ir:assignment-instruction
                 :input (first inputs)
                 :output (first outputs))
               instruction)
              (setf (first inputs) (first outputs)))))))
