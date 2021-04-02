(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric process-inputs (predecessor instruction))

(defgeneric process-outputs (predecessor instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let ((new-predecessor (process-inputs predecessor instruction)))
    (process-outputs new-predecessor instruction)))

(defmethod process-inputs (predecessor instruction)
  (let ((result predecessor))
    (loop for input in (cleavir-ir:inputs instruction)
          when (typep input 'cleavir-ir:lexical-location)
            do (setf result (ensure-in-register input result instruction)))
    result))

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:named-call-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:catch-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:bind-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:unwind-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:enclose-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  predecessor)

(defmethod process-inputs
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  predecessor)

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

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:comparison-mixin))
  predecessor)

;;; For a BINARY-OPERATION-MIXIN, we have prepared the instruction so
;;; that the first input is dead after the instruction.  That fact
;;; makes it possible to use the same register for the output as for
;;; the first input.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:binary-operation-mixin))
  predecessor)

;;; All the following methods are for instructions that will turn into
;;; named calls, so the call-site manager will access the arguments
;;; wherever they are.  Therefore, we do not need to allocate registes
;;; for them.

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:named-call-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:catch-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:bind-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:unwind-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:enclose-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  predecessor)

;;; The MEMSET-INSTRUCTION has no outputs, so we are done.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset1-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset2-instruction))
  predecessor)

(defun ensure-unattributed (predecessor instruction lexical-location)
  (let* ((pool (output-pool predecessor))
         (candidates (determine-candidates lexical-location pool)))
    (ensure-unattributed-register predecessor instruction pool candidates)))

(defun process-outputs-default (predecessor instruction)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction)))
         (pool (output-pool instruction)))
    ;; We do not need to allocate a new register if either the output
    ;; and the input are the same or the input is dead after
    ;; INSTRUCTION.
    (unless (and (typep input 'cleavir-ir:lexical-location)
                 (or (eq input output)
                     (not (member input pool
                                  :test #'eq :key #'lexical-location))))
      (ensure-unattributed predecessor instruction output))))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memref1-instruction))
  (process-outputs-default predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memref2-instruction))
  (process-outputs-default predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:assignment-instruction))
  (process-outputs-default predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:compute-argument-count-instruction))
  (ensure-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:argument-instruction))
  (process-outputs-default predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-return-values-instruction))
  nil)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:set-return-value-instruction))
  nil)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:return-instruction))
  nil)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:load-constant-instruction))
  (ensure-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defun allocate-registers-for-instructions (mir)
  (labels ((process-pair (predecessor instruction)
             (if (null (input-arrangement instruction))
                 (progn (allocate-registers-for-instruction
                         predecessor instruction)
                        (loop for successor in (cleavir-ir:successors instruction)
                              do (process-pair instruction successor)))
                 ;; FIXME: adapt the output arrangement of PREDECESSOR
                 ;; to the existing input arrangement of INSTRUCTION.
                 nil)))
    (let* ((outputs (cleavir-ir:outputs mir))
           (static-environment-location (first outputs))
           (dynamic-environment-location (second outputs)))
      (setf (output-arrangement mir)
            (make-instance 'arrangement
              :register-map *initial*
              :stack-map #*
              :attributions
              (list
               (make-instance 'attribution
                 :lexical-location static-environment-location
                 :stack-slot nil
                 :register-number 10)
               (make-instance 'attribution
                 :lexical-location dynamic-environment-location
                 :stack-slot nil
                 :register-number 1))))
      (process-pair mir (first (cleavir-ir:successors mir))))))
