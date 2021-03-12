(cl:in-package #:sicl-register-allocation)

;;; By SPILL, we mean to copy a particular register R to any free
;;; stack slot.  Since a register can be attributed to several lexical
;;; locations, in a particular arrangement, there can be several
;;; attributions with the same information about register and stack
;;; slot.  For that reason, we must find every attribution with R in
;;; it and provide modified attributions in the new arrangement.  The
;;; spill is made explicit in the MIR code as an
;;; ASSIGNMENT-INSTRUCTION I with the same lexical location in the
;;; input and the output.  The input arrangement of I contains one or
;;; more attributions with R in them and with a NIL stack slot.  The
;;; output arrangement of I is similar to the input arrangement, but
;;; with the attributions containing R having a valid stack slot
;;; number in them.

(defun spill (predecessor instruction register)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (selected-attributions
           (remove register attributions :test-not #'eq :key #'register))
         (remaining-attributions
           (set-difference attributions selected-attributions :test #'eq))
         (lexical-location (lexical-location (first selected-attributions)))
         (stack-map (stack-map arrangement))
         (length (length stack-map))
         (available-stack-slot (position 0 stack-map))
         (new-length (+ length (if (null available-stack-slot) 1 0)))
         (new-stack-map (make-array new-length :element-type 'bit)))
    (replace new-stack-map stack-map)
    (when (null available-stack-slot)
      (setf available-stack-slot length))
    (setf (bit new-stack-map available-stack-slot) 1)
    (let* ((new-attributions
             (loop for selected-attribution in selected-attributions
                   collect (make-instance 'attribution
                             :lexical-location lexical-location
                             :register register
                             :stack-slot available-stack-slot)))
           (new-arrangement (make-instance 'arrangement
                              :stack-map new-stack-map
                              :attributions
                              (append new-attributions remaining-attributions)))
           (new-instruction (make-instance 'cleavir-ir:assignment-instruction
                              :input lexical-location
                              :output lexical-location)))
      (cleavir-ir:insert-instruction-between
       new-instruction predecessor instruction)
      (setf (input-arrangement new-instruction) arrangement
            (output-arrangement new-instruction) new-arrangement)
      new-instruction)))

;;; By UNSPILL, we mean to copy a particular stack slot to a register.
;;; As with the SPILL, several attributions may be involved.  And as
;;; with SPILL, the UNSPILL is made explicit with an
;;; ASSIGNMENT-INSTRUCTION with the analogous input and output
;;; arrangements.
