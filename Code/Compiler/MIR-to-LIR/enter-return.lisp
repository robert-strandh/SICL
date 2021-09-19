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
    (setf predecessor
          (grow-stack-frame predecessor *stack-slots*))
    (install-call-site-descriptor predecessor successor)))

(defun save-return-address (predecessor successor)
  ;; If there are arguments on the stack, the return address will be
  ;; on the top of the stack, rather than in its slot.
  (let ((successor
          (make-instance 'cleavir-ir:nop-instruction
            :inputs '()
            :outputs '()
            :successors (list successor)))
        (instruction
          (make-instance 'cleavir-ir:fixnum-less-instruction
            :inputs (list x86-64:*argument-count*
                          (cleavir-ir:make-immediate-input
                           (* 2 (1+ (length x86-64:*argument-registers*)))))
            :outputs '())))
    (cleavir-ir:insert-instruction-after instruction predecessor)
    (setf (cleavir-ir:successors instruction)
          (list successor
                (make-instance 'sicl-ir:pop-instruction
                  :inputs '()
                  :outputs (list (sicl-ir:effective-address
                                  x86-64:*rbp*
                                  :offset -8))
                  :successors (list successor))))
    successor))

(defun grow-stack-frame (predecessor slots)
  (cond
    ((zerop slots)
     predecessor)
    (t
     (let ((instruction
             (make-instance 'cleavir-ir:fixnum-sub-instruction
               :inputs (list x86-64:*rsp*
                             (cleavir-ir:make-immediate-input (* slots 8)))
               :outputs (list x86-64:*rsp*))))
       (cleavir-ir:insert-instruction-after instruction predecessor)
       instruction))))

(defun spill-arguments (predecessor successor)
  ;; We extend the stack to contain the first five arguments and the
  ;; argument count (to allow for precise collection of arguments).
  (setf predecessor
        (grow-stack-frame predecessor
                          (1+ (length x86-64:*argument-registers*))))
  (loop for register in (cons x86-64:*argument-count*
                              x86-64:*argument-registers*)
        for slot from 0
        for instruction = (make-instance 'cleavir-ir:memset2-instruction
                            :inputs (list x86-64:*rsp*
                                          (cleavir-ir:make-immediate-input
                                           (* slot 8))
                                          register)
                            :outputs '())
        do (cleavir-ir:insert-instruction-between
            instruction
            predecessor
            successor)
           (setf predecessor instruction))
  predecessor)

(defun install-call-site-descriptor (predecessor successor)
  (declare (ignore successor))
  predecessor)
