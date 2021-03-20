(cl:in-package #:sicl-register-allocation)

;;; By SPILL, we mean to copy a particular register R to any free
;;; stack slot.  The spill is made explicit in the MIR code as an
;;; ASSIGNMENT-INSTRUCTION I with the same lexical location in the
;;; input and the output.  The input arrangement of I contains an
;;; attribution with R in it and with a NIL stack slot.  The output
;;; arrangement of I is similar to the input arrangement, but with the
;;; attribution containing R having a valid stack slot number in it.

(defun spill (predecessor instruction register)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (selected-attribution
           (find register attributions :test #'eq :key #'register))
         (remaining-attributions
           (remove selected-attribution attributions :test #'eq))
         (lexical-location (lexical-location selected-attribution))
         (stack-map (stack-map arrangement))
         (length (length stack-map))
         (available-stack-slot (position 0 stack-map))
         (new-length (+ length (if (null available-stack-slot) 1 0)))
         (new-stack-map (make-array new-length :element-type 'bit)))
    (replace new-stack-map stack-map)
    (when (null available-stack-slot)
      (setf available-stack-slot length))
    (setf (bit new-stack-map available-stack-slot) 1)
    (let* ((new-attribution
             (make-instance 'attribution
               :lexical-location lexical-location
               :register register
               :stack-slot available-stack-slot))
           (new-arrangement (make-instance 'arrangement
                              :stack-map new-stack-map
                              :attributions
                              (cons new-attribution remaining-attributions)))
           (new-instruction (make-instance 'cleavir-ir:assignment-instruction
                              :input lexical-location
                              :output lexical-location)))
      (cleavir-ir:insert-instruction-between
       new-instruction predecessor instruction)
      (setf (input-arrangement new-instruction) arrangement
            (output-arrangement new-instruction) new-arrangement)
      new-instruction)))

;;; By UNSPILL, we mean to copy a particular stack slot to a register.
;;; As with SPILL, the UNSPILL is made explicit with an
;;; ASSIGNMENT-INSTRUCTION with the analogous input and output
;;; arrangements.

(defun unspill (predecessor instruction stack-slot register)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (selected-attribution
           (find stack-slot attributions :test #'eql :key #'stack-slot))
         (remaining-attributions
           (remove selected-attribution attributions :test #'eq))
         (lexical-location (lexical-location selected-attribution))
         (stack-map (stack-map arrangement))
         (length (length stack-map))
         (new-stack-map (make-array length :element-type 'bit)))
    (replace new-stack-map stack-map)
    (let* ((new-attribution
             (make-instance 'attribution
               :lexical-location lexical-location
               :register register
               :stack-slot stack-slot))
           (new-arrangement (make-instance 'arrangement
                              :stack-map new-stack-map
                              :attributions
                              (cons new-attribution remaining-attributions)))
           (new-instruction (make-instance 'cleavir-ir:assignment-instruction
                              :input lexical-location
                              :output lexical-location)))
      (cleavir-ir:insert-instruction-between
       new-instruction predecessor instruction)
      (setf (input-arrangement new-instruction) arrangement
            (output-arrangement new-instruction) new-arrangement)
      new-instruction)))
