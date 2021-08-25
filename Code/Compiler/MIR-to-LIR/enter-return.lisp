(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:return-instruction))
  ;; As per section 27.3 of the SICL specification, we shrink the
  ;; frame so that the return address will be at the hot end of the
  ;; frame before using the RET instruction.
  (cleavir-ir:insert-instruction-before
   (make-instance 'sicl-ir:load-effective-address-instruction
     :inputs (list (sicl-ir:effective-address x86-64:*rbp* :displacement -8))
     :outputs (list x86-64:*rsp*))
   instruction))

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:enter-instruction))
  (let ((predecessor instruction)
        (successor (cleavir-ir:first-successor instruction)))
    (setf predecessor
          (save-return-address predecessor successor))
    (when *spill-arguments-p*
      (setf predecessor
            (spill-arguments predecessor successor)))
    (install-call-site-descriptor predecessor successor)))

(defun save-return-address (predecessor successor)
  ;; If there are arguments on the stack, the return address will be
  ;; on the top of the stack, rather than in its slot.
  (let ((instruction
          (make-instance 'cleavir-ir:fixnum-less-instruction
            :inputs (list x86-64:*argument-count*
                          (cleavir-ir:make-immediate-input
                           (* 2 (1+ (length x86-64:*argument-registers*)))))
            :outputs '()
            :successors (list successor
                              (make-instance 'sicl-ir:pop-instruction
                                :inputs '()
                                :outputs (list (sicl-ir:effective-address
                                                x86-64:*rbp*
                                                :offset -8)))))))
    (cleavir-ir:insert-instruction-between instruction predecessor successor)
    instruction))

(defun spill-arguments (predecessor successor)
  predecessor)
(defun install-call-site-descriptor (predecessor successor)
  predecessor)
