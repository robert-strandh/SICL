 (cl:in-package #:sicl-mir-to-lir)

;;; We need to store arguments into the appropriate registers and
;;; stack slots (relative to RSP now), and then adjust RBP.

;;; The code generated is as per the description in section 27.3
;;; "Calling conventions" of the SICL specification, though we have
;;; changed the order of some independent steps to make code
;;; generation easier.  The callee function object and arguments have
;;; already been computed by previous instructions, so we are left to
;;; generate code for the rest of the process.

;;; TODO: put these magic numbers into another system.
(defconstant +rack-prefix-size+ 2)
(defconstant +rack-tag+ 7)
(defconstant +standard-object-tag+ 5)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:funcall-instruction))
  (destructuring-bind (function &rest arguments)
      (cleavir-ir:inputs instruction)
    (let* ((argument-count (length arguments))
           (start-of-sequence (make-instance 'cleavir-ir:nop-instruction))
           (predecessor start-of-sequence)
           (function-register x86-64:*r11*)
           (scratch-register x86-64:*rax*))
      (cleavir-ir:insert-instruction-before
       start-of-sequence
       instruction)
      (flet ((emit (instruction)
               (cleavir-ir:insert-instruction-after
                instruction
                predecessor)
               (setf predecessor instruction)))
        ;; 1. Load the rack of the function into a register.
        (emit
         (x86-64:load-from-location-instruction function function-register))
        (emit
         (make-instance 'cleavir-ir:memref2-instruction
           :inputs (list function-register
                         (cleavir-ir:make-immediate-input (- 8 +standard-object-tag+)))
           :outputs (list function-register)))
        ;; 2. Store the (boxed) argument count in R9.
        (emit
         (make-instance 'cleavir-ir:assignment-instruction
           :inputs (list (cleavir-ir:make-immediate-input (ash argument-count 1)))
           :outputs (list x86-64:*r9*)))
        ;; 3. Store the arguments in RDI, RSI, RDX, RCX, and R8.
        (loop for argument in arguments
              for register in x86-64:*argument-registers*
              do (x86-64:load-from-location-instruction argument
                                                        register))
        (cond
          ((<= argument-count (length x86-64:*argument-registers*))
           ;; 4. Push the value of RBP on the stack.
           (emit
            (make-instance 'sicl-ir:push-instruction
              :inputs (list x86-64:*rbp*)))
           ;; 5. Copy the value of RSP into RBP.
           (emit
            (make-instance 'cleavir-ir:assignment-instruction
              :inputs (list x86-64:*rsp*)
              :outputs (list x86-64:*rbp*))))
          (t
           ;; 4. Subtract 8(N - 3) from RSP, where N is the number of
           ;; arguments to pass.
           (emit
            (make-instance 'cleavir-ir:fixnum-sub-instruction
              :inputs (list x86-64:*rsp*
                            (cleavir-ir:make-immediate-input
                             (* 8 (- argument-count 3))))
              :outputs (list x86-64:*rsp*)))
           ;; 5. Store the remaining arguments relative to RSP.
           (loop for stack-slot from 0
                 for argument in (nthcdr (length x86-64:*argument-registers*)
                                         arguments)
                 for register = (etypecase argument
                                  (cleavir-ir:register-location
                                   argument)
                                  (cleavir-ir:stack-location
                                   (emit
                                    (x86-64:load-from-location-instruction
                                     argument
                                     scratch-register))
                                   scratch-register))
                 do (emit
                     (make-instance 'cleavir-ir:memset2-instruction
                       :inputs (list x86-64:*rsp*
                                     (cleavir-ir:make-immediate-input
                                      (* 8 stack-slot))
                                     register))))
           ;; 5.25. Store the value of RBP into [RSP + 8(N - 4)].
           (emit
            (make-instance 'cleavir-ir:memset2-instruction
              :inputs (list x86-64:*rbp*
                            x86-64:*rsp*
                            (cleavir-ir:make-immediate-input
                             (* 8 (- argument-count 4))))))
           ;; 5.5 Copy the value of RSP + 8(N - 4) into RBP. TODO: use
           ;; LEA to do this.
           (emit
            (make-instance 'cleavir-ir:assignment-instruction
              :inputs (list x86-64:*rsp*)
              :outputs (list x86-64:*rbp*)))
           (emit
            (make-instance 'cleavir-ir:fixnum-add-instruction
              :inputs (list x86-64:*rbp*
                            (cleavir-ir:make-immediate-input
                             (* 8 (- argument-count 4))))))))
        ;; 6. Load the static environment of the callee from the
        ;; callee function object into R10.  As per
        ;; CLOS/funcallable-standard-object-defclass.lisp the
        ;; environment is in the second slot of a
        ;; funcallable-standard-object.
        (emit
         (make-instance 'cleavir-ir:memref2-instruction
           :inputs (list function-register
                         (cleavir-ir:make-immediate-input
                          (- (* 8 (+ +rack-prefix-size+ 1))
                             +rack-tag+)))
           :outputs (list x86-64:*r10*)))
        ;; 7. Load the entry point address of the callee into an
        ;; available scratch register, typically RAX.
        (emit
         (make-instance 'cleavir-ir:memref2-instruction
           :inputs (list function-register
                         (cleavir-ir:make-immediate-input
                          (- (* 8 (+ +rack-prefix-size+ 0))
                             +rack-tag+)))
           :outputs (list x86-64:*rax*)))
        ;; 8. Use the CALL instruction with that register as an argument.
        ;; We will just reuse this CALL instruction.
        (setf (cleavir-ir:inputs instruction)
              (list x86-64:*rax*)))
      ;; Remove the NOP instruction.
      (cleavir-ir:delete-instruction start-of-sequence))))

;;; However, we don't do anything before a named call.

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:named-call-instruction))
  nil)
