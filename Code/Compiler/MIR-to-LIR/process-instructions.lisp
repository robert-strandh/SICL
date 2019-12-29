(cl:in-package #:sicl-mir-to-lir)

(defgeneric process-instruction (instruction lexical-locations))

(defmethod process-instruction (instruction lexical-locations)
  nil)

(defun insert-memref-before
    (instruction lexical-location register lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (+ (gethash lexical-location lexical-locations) 8))))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rbp*
       :output *r11*)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :inputs (list *r11* immediate-input)
       :output *r11*)
     instruction)
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:memref1-instruction
       :input *r11*
       :output register)
     instruction)))

(defun insert-memset-after
    (instruction register lexical-location lexical-locations)
  (let ((immediate-input
          (make-instance 'cleavir-ir:immediate-input
            :value (* (1+ (gethash lexical-location lexical-locations)) 8))))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:memset1-instruction
       :inputs (list *r11* register)
       :output register)
     instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :inputs (list *r11* immediate-input)
       :output *r11*)
     instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rbp*
       :output *r11*)
     instruction)))

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
                lexical-locations)
               (setf (first outputs) register))))
