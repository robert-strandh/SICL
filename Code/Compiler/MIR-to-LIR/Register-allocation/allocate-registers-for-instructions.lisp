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
  (with-arrangement-parts (stack-map register-map attributions arrangement)
    (let* ((new-stack-map (copy-stack-map stack-map))
           (new-register-map (copy-register-map register-map))
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
        :attributions new-attributions))))

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
    (with-arrangement-parts
        (stack-map register-map attributions (output-arrangement instruction))
      (let* ((input-arrangement (input-arrangement instruction))
             (new-register-map (copy-register-map register-map))
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
                :attributions (cons new-attribution attributions)))))))

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

(defun allocate-and-attribute-new-register
    (predecessor instruction lexical-location)
  (let* ((input-pool (input-pool instruction))
         (output-pool (output-pool instruction))
         (output-arrangement (output-arrangement instruction))
         (candidates (determine-candidates lexical-location output-pool)))
    (multiple-value-bind (new-predecessor register-number)
        (allocate-register predecessor instruction input-pool candidates)
      (declare (ignore new-predecessor))
      (setf (output-arrangement instruction)
            (update-arrangement-for-new-definition
             output-arrangement lexical-location register-number)))))

;;;
(defun allocate-or-reuse
    (predecessor instruction input-attribution output-location pool)
  (let ((input-location (lexical-location input-attribution)))
    (if (or (eq input-location output-location)
            (not (member input-location pool
                         :test #'eq :key #'lexical-location)))
        ;; Then we can reuse the same register
        (setf (output-arrangement instruction)
              (update-arrangement-for-new-definition
               (output-arrangement instruction)
               output-location
               (register-number input-attribution)))
        ;; Otherwise, we need to allocate a new register and
        ;; attribute it to the output location.
        (allocate-and-attribute-new-register
         predecessor instruction output-location))))

(defun compute-output-arrangement-default (predecessor instruction)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (input-arrangement (input-arrangement instruction))
         (input-attributions (attributions input-arrangement))
         (input-attribution (find input input-attributions
                                  :test #'eq :key #'lexical-location))
         (output (first (cleavir-ir:outputs instruction)))
         (pool (output-pool instruction)))
    (allocate-or-reuse
     predecessor instruction input-attribution output pool)))

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:memref1-instruction))
  (compute-output-arrangement-default predecessor instruction))

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:memref2-instruction))
  (compute-output-arrangement-default predecessor instruction))

(defmethod compute-output-arrangement
    (predecessor (instruction cleavir-ir:assignment-instruction))
  (compute-output-arrangement-default predecessor instruction))
