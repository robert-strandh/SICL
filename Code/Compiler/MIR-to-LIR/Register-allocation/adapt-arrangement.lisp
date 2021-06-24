(in-package #:sicl-register-allocation)

;;; An instruction which is a placeholder for replacement later into
;;; MIR-to-LIR.  A later pass after registers are introduced will
;;; replace an ADAPT-INSTRUCTION with instructions to adapt between
;;; arrangements.
(defclass adapt-instruction (cleavir-ir:instruction)
  ())

(defun adapt-arrangements (predecessor instruction)
  (cond
    ((arr:arrangements-compatible-p
      (output-arrangement predecessor)
      (input-arrangement instruction))
     nil)
    (t
     (let ((adapt-instruction (make-instance 'adapt-instruction)))
       (setf (input-arrangement adapt-instruction)
             (output-arrangement predecessor)
             (output-arrangement adapt-instruction)
             (input-arrangement instruction))
       (cleavir-ir:insert-instruction-between
        adapt-instruction
        predecessor instruction)))))
