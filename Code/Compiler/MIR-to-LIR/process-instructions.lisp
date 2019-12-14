(cl:in-package #:sicl-mir-to-lir)

(defgeneric process-instruction (instruction lexical-locations))

(defmethod process-instruction (instruction lexical-locations)
  (declare (ignore lexical-locations))
  (error "Don't know how to process instruction ~s" instruction))

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
            :value (+ (gethash lexical-location lexical-locations) 8))))
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
