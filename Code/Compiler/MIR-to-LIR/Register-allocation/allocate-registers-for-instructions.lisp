(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric compute-input-arrangement (predecessor instruction))

(defmethod compute-input-arrangement :after (predecessor instruction)
  (setf (input-arrangement instruction)
        (output-arrangement predecessor)))

(defgeneric compute-output-arrangement (predecessor instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let ((new-predecessor (compute-input-arrangement predecessor instruction)))
    (compute-output-arrangement new-predecessor instruction)))

(defmethod compute-input-arrangement (predecessor instruction)
  (let ((result predecessor))
    (loop for input in (cleavir-ir:inputs instruction)
          when (typep input 'cleavir-ir:lexical-location)
            do (setf result (ensure-in-register input result instruction)))))

;;; Create a new arrangement that is like ARRANGEMENT but keeping only
;;; attributions of live lexical locations as indicated by the fact
;;; that they appear in POOL.
(defun filter-arrangement (arrangement pool)
  (let* ((stack-map (stack-map arrangement))
         (new-stack-map (copy-stack-map stack-map))
         (register-map (register-map arrangement))
         (new-register-map (copy-register-map register-map))
         (attributions (attributions arrangement))
         (new-attributions '()))
    (loop for attribution in attributions
          for location = (lexical-location attribution)
          for stack-slot = (stack-slot attribution)
          for register-number = (register-number attribution)
          do (if (member location pool :test #'eq :key #'lexical-location)
                 (push location new-attributions)
                 (progn (unless (null stack-slot)
                          (free-stack-slot new-stack-map stack-slot))
                        (unless (null register-number)
                          (free-register new-register-map register-number)))))
    (make-instance 'arrangement
      :stack-map new-stack-map
      :register-map new-register-map
      :attributions new-attributions)))

(defmethod compute-output-arrangement :before
    (predecessor instruction)
  (setf (output-arrangement instruction)
        (filter-arrangement
         (input-arrangement instruction) (output-pool instruction))))

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:comparison-mixin))
  (declare (ignore predecessor))
  nil)

;;; For a BINARY-OPERATION-MIXIN, we have prepared the instruction so
;;; that the first input is dead after the instruction.  That fact
;;; makes it possible to use the same register for the output as for
;;; the first input.
(defmethod comput-output-arrangement
    (predecessor (instruction cleavir-ir:binary-operation-mixin))
  (let ((first-input (first (cleavir-ir:inputs instruction)))
        (output (first (cleavir-ir:outputs instruction))))
    (assert (not (eq first-input output)))
    (let* ((input-arrangement (input-arrangement instruction))
           (output-arrangement (output-arrangement instruction))
           (stack-map (stack-map output-arrangement))
           (register-map (register-map output-arrangement))
           (new-register-map (copy-register-map register-map))
           (attributions (attributions output-arrangement))
           (attribution
             (find first-input (attributions input-arrangement)
                   :test #'eq :key #'lexical-location))
           (register-number (register-number attribution))
           (new-attribution
             (make-instance 'attribution
               :lexical-location output
               :stack-slot nil
               :register-number register-number)))
      (reserve-register new-register-map register-number)
      (setf (output-arrangement instruction)
            (make-instance 'arrangement
              :stack-map stack-map
              :register-map new-register-map
              :attributions (cons new-attribution attributions))))))

;;; All the following methods are for instructions that will turn into
;;; named calls, so the call-site manager will access the arguments
;;; wherever they are.  Therefore, we do not need to allocate registes
;;; for them.

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:named-call-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:catch-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:bind-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:unwind-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:enclose-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  (declare (ignore predecessor))
  nil)

;;; The MEMSET-INSTRUCTION has no outputs, so we are done.
(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:memset1-instruction))
  (declare (ignore predecessor))
  nil)

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:memset2-instruction))
  (declare (ignore predecessor))
  nil)
