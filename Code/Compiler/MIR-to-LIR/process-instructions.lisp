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

(defmethod process-instruction :before (instruction lexical-locations)
  (unless (typep instruction 'cleavir-ir:enter-instruction)
    (loop for inputs = (cleavir-ir:inputs instruction)
            then (rest inputs)
          for register in (list *r11* *r12*)
          until (null inputs)
          unless (or (typep (first inputs) 'cleavir-ir:register-location)
                     (typep (first inputs) 'cleavir-ir:immediate-input))
            do (insert-memref-before
                instruction
                (first inputs)
                register
                *r11*
                lexical-locations)
               (setf (first inputs) register))
    (loop for outputs = (cleavir-ir:outputs instruction)
            then (rest outputs)
          for register in (list *r11* *r12*)
          until (null outputs)
          unless (typep (first outputs) 'cleavir-ir:register-location)
            do (insert-memset-after
                instruction
                register
                (first outputs)
                *r11*
                lexical-locations)
               (setf (first outputs) register))))
