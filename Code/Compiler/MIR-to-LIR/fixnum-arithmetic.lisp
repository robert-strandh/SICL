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
  (let ((quotient (first (cleavir-ir:outputs instruction))))
    (cleavir-ir:insert-instruction-after
     (make-instance 'cleavir-ir:shift-left-instruction
       :inputs (list quotient (cleavir-ir:make-immediate-input 1))
       :outputs (list quotient))
     instruction)))
