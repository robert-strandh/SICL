 (cl:in-package #:sicl-mir-to-lir)

;;; We need to store arguments into the appropriate registers and
;;; stack slots (relative to RSP now), and then adjust RBP.

;;; The code generated is as per the description in section 27.3
;;; "Calling conventions" of the SICL specification.  The callee
;;; function object and arguments have already been computed by
;;; previous instructions, so we are left to generate code for the
;;; rest of the process.

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
           (function-register x86-64:*r11*))
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
                         (- 8 +standard-object-tag+))
           :outputs (list function-register)))
        (cond
          ((<= argument-count (length x86-64:*argument-registers*))
           ;; 2. Store the arguments in RDI, RSI, RDX, RCX, and R8.
           (loop for argument in arguments
                 for register in x86-64:*argument-registers*
                 do (x86-64:load-from-location-instruction argument
                                                           register))
           ;; 3. Store the argument count in R9.
           (emit
            (make-instance 'cleavir-ir:assignment-instruction
              :inputs (list (make-instance 'cleavir-ir:immediate-input
                              :value argument-count))
              :outputs (list x86-64:*r9*)))
           ;; 4. Load the static environment of the callee from the
           ;; callee function object into R10.  As per
           ;; CLOS/funcallable-standard-object-defclass.lisp the
           ;; environment is in the second slot of a
           ;; funcallable-standard-object.
           (emit
            (make-instance 'cleavir-ir:memref2-instruction
              :inputs (list function-register
                            (- (* 8 (+ +rack-prefix-size+ 1))
                               +rack-tag+))
              :outputs (list x86-64:*r10*)))
           ;; 5. Push the value of RBP on the stack.
           (emit
            (make-instance 'sicl-ir:push-instruction
              :inputs (list x86-64:*rbp*)))
           ;; 6. Copy the value of RSP into RBP.
           (emit
            (make-instance 'cleavir-ir:assignment-instruction
              :inputs (list x86-64:*rsp*)
              :outputs (list x86-64:*rbp*)))
           ;; 7. Load the entry point address of the callee into an
           ;; available scratch register, typically RAX.
           (emit
            (make-instance 'cleavir-ir:memref2-instruction
              :inputs (list function-register
                            (- (* 8 (+ +rack-prefix-size+ 0))
                               +rack-tag+))
              :outputs (list x86-64:*rax*)))
           ;; 8. Use the CALL instruction with that register as an argument.
           ;; We will just reuse this CALL instruction.
           (setf (cleavir-ir:inputs instruction)
                 (list x86-64:*rax*)))
          (t
           (error "Can't handle more than 5 arguments so far.")))))))

;;; However, we don't do anything before a named call.

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:named-call-instruction))
  nil)
