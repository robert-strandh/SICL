(cl:in-package #:sicl-mir-to-lir)

(defmethod process-instruction
    ((instruction cleavir-ir:initialize-return-values-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :outputs (list *rdi*)))

;;; The input representing the index of the value can be either
;;; immediate or lexical.  When it is a lexical input, it is not one
;;; of the 5 registers used for multiple return values, but is
;;; necessarily an offset to be subtracted from RSP.  When it is an
;;; immediate input, it can be either one of the registers, or it can
;;; be an offset into the stack.  The offset is a fixnum, so it has to
;;; be converted to a byte offset.

(defun set-return-value-instruction-with-lexical-input
    (instruction index-input value-location lexical-locations)
  ;; Load the value of the lexical input into register R11.
  (load-lexical index-input *r11* instruction lexical-locations)
  ;; Subtract 4*2 from R11 to get the offset (in words) to subtract
  ;; from RSP in order to get to the value location in question.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-sub-instruction
     :inputs (list *r11* (make-instance 'cleavir-ir:immediate-input :value 8))
     :output *r11*)
   instruction)
  ;; Shift R11 left by 3 positions to convert the word offset to a
  ;; byte offset.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:shift-left-instruction
     :inputs (list *r11* (make-instance 'cleavir-ir:immediate-input :value 3))
     :output *r11*)
   instruction)
  ;; Negate the value of R11 so that we can later add RSP to it.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:negate-instruction
     :input *r11*
     :output *r11*)
   instruction)
  ;; Add RSP to R11.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-add-instruction
     :inputs (list *r11* *rsp*)
     :output *r11*)
   instruction)
  ;; In R11, we now have the the address of the value location in
  ;; question.
  (if (lexical-p value-location)
      ;; FIXME: using RAX here is very likely wrong since it is the
      ;; one holding the first return value.
      (progn (change-class instruction 'cleavir-ir:memset1-instruction
                           :address *r11*
                           :value *rax*)
             (insert-memref-before
              instruction
              value-location
              *rax*
              *r9*
              lexical-locations))
      (change-class instruction 'cleavir-ir:memset1-instruction
                    :address *r11*
                    :value value-location)))

(defun set-return-value-instruction-with-immediate-input
    (instruction index-input value-location lexical-locations)
  (let ((index (ash (cleavir-ir:value index-input) -1)))
    (if (< index 5)
        (let ((register
                (case index (0 *rax*) (1 *rdx*) (2 *rcx*) (3 *rsi*) (4 *r9*))))
          (if (lexical-p value-location)
              (progn (insert-memref-before
                      instruction
                      value-location
                      register
                      *r11*
                      lexical-locations)
                     (change-class instruction 'cleavir-ir:nop-instruction
                                   :inputs '()
                                   :outputs '()))
              (change-class instruction 'cleavir-ir:assignment-instruction
                            :inputs (list value-location)
                            :outputs (list register))))
        (let ((offset-input (make-instance 'cleavir-ir:immediate-input
                              :value (* 8 (- index 5)))))
          (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:assignment-instruction
             :input *rsp*
             :output *r11*)
           instruction)
          (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:unsigned-sub-instruction
             :inputs (list *r11* offset-input)
             :output *r11*)
           instruction)
          (if (lexical-p value-location)
              ;; FIXME: using RAX here is very likely wrong since it is the
              ;; one holding the first return value.
              (progn (change-class instruction 'cleavir-ir:memset1-instruction
                                   :address *r11*
                                   :value *rax*)
                     (insert-memref-before
                      instruction
                      value-location
                      *rax*
                      *r9*
                      lexical-locations))
              (change-class instruction 'cleavir-ir:memset1-instruction
                            :address *r11*
                            :value value-location))))))

(defmethod process-instruction
    ((instruction cleavir-ir:set-return-value-instruction)
     lexical-locations)
  (destructuring-bind (index-input value-location)
      (cleavir-ir:inputs instruction)
    (if (typep index-input 'cleavir-ir:immediate-input)
        (set-return-value-instruction-with-immediate-input
         instruction index-input value-location lexical-locations)
        (set-return-value-instruction-with-lexical-input
         instruction index-input value-location lexical-locations))))

(defmethod process-instruction
    ((instruction cleavir-ir:compute-return-value-count-instruction)
     lexical-locations)
  (change-class instruction 'cleavir-ir:assignment-instruction
                :inputs (list *rdi*)))

(defun return-value-instruction-with-lexical-input
    (instruction input output lexical-locations)
  ;; Load the value of the lexical input into register R11.
  (load-lexical input *r11* instruction lexical-locations)
  ;; Subtract 4*2 from R11 to get the offset (in words) to subtract
  ;; from RSP in order to get to the value in question.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-sub-instruction
     :inputs (list *r11* (make-instance 'cleavir-ir:immediate-input :value 8))
     :output *r11*)
   instruction)
  ;; Shift R11 left by 3 positions to convert the word offset to a
  ;; byte offset.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:shift-left-instruction
     :inputs (list *r11* (make-instance 'cleavir-ir:immediate-input :value 3))
     :output *r11*)
   instruction)
  ;; Negate the value of R11 so that we can later add RSP to it.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:negate-instruction
     :input *r11*
     :output *r11*)
   instruction)
  ;; Add RSP to R11.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:unsigned-add-instruction
     :inputs (list *r11* *rsp*)
     :output *r11*)
   instruction)
  ;; In R11, we now have the the address of the value in question.
  (if (lexical-p output)
      (progn (change-class instruction 'cleavir-ir:memref1-instruction
                           :inputs (list *r11*)
                           :outputs (list *r11*))
             (insert-memset-after
              instruction
              *r11*
              output
              *r9*
              lexical-locations))
      (change-class instruction 'cleavir-ir:memref1-instruction
                    :address *r11*)))

(defun return-value-instruction-with-immediate-input
    (instruction input output lexical-locations)
  (let ((index (ash (cleavir-ir:value input) -1)))
    (if (< index 5)
        (let ((register
                (case index (0 *rax*) (1 *rdx*) (2 *rcx*) (3 *rsi*) (4 *r9*))))
          (if (lexical-p output)
              (progn (insert-memset-after
                      instruction
                      register
                      output
                      *r11*
                      lexical-locations)
                     (change-class instruction 'cleavir-ir:nop-instruction
                                   :inputs '()
                                   :outputs '()))
              (change-class instruction 'assignment-instruction
                            :inputs (list register))))
        (let ((offset-input (make-instance 'cleavir-ir:immediate-input
                              :value (* 8 (- index 4)))))
          (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:assignment-instruction
             :input *rsp*
             :output *r11*)
           instruction)
          (cleavir-ir:insert-instruction-before
           (make-instance 'cleavir-ir:unsigned-sub-instruction
             :inputs (list *r11* offset-input)
             :output *r11*)
           instruction)
          (if (lexical-p output)
              ;; FIXME: using RAX here is very likely wrong since it is the
              ;; one holding the first return value.
              (progn (change-class instruction 'cleavir-ir:memref1-instruction
                                   :address *r11*
                                   :value *rax*)
                     (insert-memset-after
                      instruction
                      *rax*
                      output
                      *r9*
                      lexical-locations))
              (change-class instruction 'cleavir-ir:memref1-instruction
                            :address *r11*))))))

(defmethod process-instruction
    ((instruction cleavir-ir:return-value-instruction)
     lexical-locations)
  (let ((input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (if (typep input 'cleavir-ir:immediate-input)
        (return-value-instruction-with-immediate-input
         instruction input output lexical-locations)
        (return-value-instruction-with-lexical-input
         instruction input output lexical-locations))))
