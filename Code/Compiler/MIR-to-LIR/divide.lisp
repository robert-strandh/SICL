(cl:in-package #:sicl-mir-to-lir)

;;; FIXME: check this translation!!!!

(defmethod process-instruction
    ((instruction cleavir-ir:fixnum-divide-instruction) lexical-locations)
  (let ((inputs (cleavir-ir:inputs instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:assignment-instruction
       :input (make-instance 'cleavir-ir:immediate-input :value 0)
       :output *rdx*)
     instruction)
    (cond ((lexical-p (first inputs))
           (insert-memref-before
            instruction
            (first inputs)
            *rax*
            lexical-locations))
          ((eq (first inputs) *rax*)
           nil)
          (t
           (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:assignment-instruction
              :input (first inputs)
              :output *rax*)
            instruction)))
    (cond ((lexical-p (second inputs))
           (insert-memref-before
            instruction
            (second inputs)
            *rcx*
            lexical-locations)
           (setf (second inputs) *rcx*))
          ((typep (second inputs) 'cleavir-ir:immediate-input)
           (cleavir-ir:insert-instruction-before
            (make-instance 'cleavir-ir:assignment-instruction
              :input (second inputs)
              :output *r11*)
            instruction)
           (setf (second inputs) *r11*))
          (t nil))
    (cond ((lexical-p (first outputs))
           (insert-memset-after
            instruction
            *rax*
            (first outputs)
            lexical-locations))
          ((eq (first inputs) *rax*)
           nil)
          (t
           (cleavir-ir:insert-instruction-after
            (make-instance 'cleavir-ir:assignment-instruction
              :input *rax*
              :output (first outputs))
            instruction)))
    (cond ((lexical-p (second outputs))
           (insert-memset-after
            instruction
            *rdx*
            (second outputs)
            lexical-locations))
          ((eq (second outputs) *rdx*)
           nil)
          (t
           (cleavir-ir:insert-instruction-after
            (make-instance 'cleavir-ir:assignment-instruction
              :input *rdx*
              :output (second outputs))
            instruction)))
    (setf (first inputs) *rax*)
    (setf (first outputs) *rax*)
    (setf (second outputs) *rdx*)))
        
  
