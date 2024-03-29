(cl:in-package #:sicl-mir-to-lir)

;;; We have two inputs, say, m and n, and when boxed they are 2m and
;;; 2n.  We want to get 2mn.  So we shift one of them right (the one
;;; in RAX is always safe to modify like this) to get 2(2m / 2)n =
;;; 2mn.

;;; Additionally, we need to put the multiplicand in RAX, and the most
;;; significant output needs to be shifted to box it.

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:fixnum-multiply-instruction))
  (destructuring-bind (input1 input2)
      (cleavir-ir:inputs instruction)
    ;; Make sure the first input is in RAX.
    (unless (eq input1 x86-64:*rax*)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:assignment-instruction
         :inputs (list input1)
         :outputs (list x86-64:*rax*))
       instruction))
    (cleavir-ir:insert-instruction-before
     (make-instance 'cleavir-ir:arithmetic-shift-right-instruction
       :inputs  (list x86-64:*rax*
                      (cleavir-ir:make-immediate-input 1))
       :outputs (list x86-64:*rax*))
     instruction)
    (setf (cleavir-ir:inputs instruction)
          (list x86-64:*rax* input2)))
  (let ((most-significant (first (cleavir-ir:outputs instruction))))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:shift-left-instruction
       :inputs (list most-significant (cleavir-ir:make-immediate-input 1))
       :outputs (list most-significant))
     instruction)))

;;; We want to get 2m/n.  (2m)/(2n) = m/n, so we just shift the
;;; quotient to get 2m/n.  As for the modulo, we compute 2m mod 2n =
;;; 2(m mod n) which is fine.

;;; Additionally, we need to put the dividend in RAX.

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:fixnum-divide-instruction))
  ;; Make sure the dividend is in RAX.
  (destructuring-bind (dividend divisor)
      (cleavir-ir:inputs instruction)
    (unless (eq dividend x86-64:*rax*)
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:assignment-instruction
         :inputs (list dividend)
         :outputs (list x86-64:*rax*))
       instruction)
      (setf (cleavir-ir:inputs instruction)
            (list x86-64:*rax* divisor))))
  ;; Zero out RDX before performing the division, as it is interpreted
  ;; as the high 64 bits of a 128-bit dividend.
  (cleavir-ir:insert-instruction-before
   (make-instance 'cleavir-ir:assignment-instruction
     :inputs (list (cleavir-ir:make-immediate-input 0))
     :outputs (list x86-64:*rdx*))
   instruction)
  (let ((quotient (first (cleavir-ir:outputs instruction))))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:shift-left-instruction
       :inputs (list quotient (cleavir-ir:make-immediate-input 1))
       :outputs (list quotient))
     instruction)))

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:shift-instruction))
  ;; Make sure the shift count is in RCX, or is immediate.
  (destructuring-bind (shifted-input shift-count)
      (cleavir-ir:inputs instruction)
    (unless (or (eq shift-count x86-64:*rcx*)
                (typep shift-count 'cleavir-ir:immediate-input))
      (cleavir-ir:insert-instruction-before
       (make-instance 'cleavir-ir:assignment-instruction
         :inputs (list shift-count)
         :outputs (list x86-64:*rcx*))
       instruction)
      (setf (cleavir-ir:inputs instruction)
            (list shifted-input x86-64:*rcx*)))))
