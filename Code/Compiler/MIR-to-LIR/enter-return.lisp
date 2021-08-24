(cl:in-package #:sicl-mir-to-lir)

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:return-instruction))
  ;; As per section 27.3 of the SICL specification, we shrink the
  ;; frame so that the return address will be at the hot end of the
  ;; frame before using the RET instruction.
  (cleavir-ir:insert-instruction-before
   (make-instance 'sicl-ir:load-effective-address-instruction
     :inputs (list x86-64:*rbp*
                   (cleavir-ir:make-immediate-input 0)
                   (sicl-ir:nowhere)
                   (cleavir-ir:make-immediate-input -8))
     :outputs (list x86-64:*rsp*))
   instruction))

(defun save-return-address (predecessor successor))
(defun spill-arguments (predecessor successor))

(defmethod finish-lir-for-instruction
    ((instruction cleavir-ir:enter-instruction))
  (let ((predecessor instruction)
        (successor (cleavir-ir:first-successor instruction)))
    (setf predecessor
          (save-return-address predecessor successor))
    (when *spill-arguments-p*
      (setf predecessor
            (spill-arguments predecessor successor)))
    ;; We also need to install a call-site descriptor.
    ))
