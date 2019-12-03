(cl:in-package #:sicl-mir-to-lir)

;;;; When the function is entered, the return address is on top of the
;;;; stack.  We need to move it to the address indicated by RBP-8.
;;;;
;;;; 1. Pop the stack into R11.
;;;; 2. Copy RBP to R12.
;;;; 3. Subtract 8 from R12.
;;;; 4. Store R11 to the stack at the address indicated by R12.

(defun move-return-address (enter-instruction)
  (let ((immediate-input-8
          (make-instance 'cleavir-ir:immediate-input :value 8)))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:memset1-instruction
       :inputs (list *r12* *r11*)
       :outputs '())
     enter-instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:unsigned-sub-instruction
       :inputs (list *r12* immediate-input-8)
       :output *r12*)
     enter-instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:assignment-instruction
       :input *rbp*
       :output *r12*)
     enter-instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:unsigned-add-instruction
       :input (list *rsp* immediate-input-8)
       :output *rsp*)
     enter-instruction)
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:memref1-instruction
       :input *rsp*
       :output *r11*)
     enter-instruction)))
