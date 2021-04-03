(cl:in-package #:sicl-register-allocation)

(defgeneric allocate-registers-for-instruction (predecessor instruction))

(defgeneric process-inputs (predecessor instruction))

(defgeneric process-outputs (predecessor instruction))

(defgeneric compute-output-arrangement (instruction))

(defmethod allocate-registers-for-instruction (predecessor instruction)
  (let* ((new-predecessor (process-inputs predecessor instruction))
         (new-predecessor (process-outputs new-predecessor instruction)))
    (setf (input-arrangement instruction)
          (output-arrangement new-predecessor))
    (compute-output-arrangement instruction)))

;;; By default, we make sure that every input that is a lexical
;;; location is in a register.
(defmethod process-inputs (predecessor instruction)
  (let ((result predecessor))
    (loop for input in (cleavir-ir:inputs instruction)
          when (typep input 'cleavir-ir:lexical-location)
            do (setf result (ensure-in-register input result instruction)))
    result))

;;; For named-call instructions, we do not do anything particular.
;;; The call-site manager will generate the code to fetch the
;;; arguments and put them in the location where the callee needs them
;;; to be.
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
                   (push attribution new-attributions)
                   (progn (unless (null stack-slot)
                            (free-stack-slot new-stack-map stack-slot))
                          (unless (null register-number)
                            (unmark-register new-register-map register-number)))))
      (make-instance 'arrangement
        :stack-map new-stack-map
        :register-map new-register-map
        :attributions new-attributions))))

;;; Comparison instructions don't have any outputs, so nothing needs
;;; to be done.
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
;;; wherever they are.

;;; What we need to do here is to make sure that every lexical
;;; location that
;;;   1. is live after the call,
;;;   2. is not present in a stack slot, and
;;;   3. is not in a callee-saves register,
;;; is spilled before the call.
(defun process-outputs-for-named-call (predecessor instruction)
  (let ((result predecessor))
    (loop with arrangement = (output-arrangement predecessor)
          with pool = (output-pool instruction)
          for attribution in (attributions arrangement)
          for location = (lexical-location attribution)
          for register-number = (register-number attribution)
          when (and (member location pool
                            :test #'eq :key #'lexical-location)
                    (null (stack-slot attribution))
                    (not (register-number-is-callee-saves-p register-number)))
            do (setf result
                     (spill result instruction register-number)))
    result))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:named-call-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:catch-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:bind-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:unwind-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:enclose-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-values-instruction))
  (process-outputs-for-named-call predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:multiple-value-call-instruction))
  (process-outputs-for-named-call predecessor instruction))

;;; The MEMSET-INSTRUCTION has no outputs, so we are done.
(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset1-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:memset2-instruction))
  predecessor)

(defun ensure-unattributed (predecessor instruction lexical-location)
  (let* ((pool (output-pool instruction))
         (candidates (determine-candidates lexical-location pool)))
    (ensure-unattributed-register
     predecessor instruction (output-pool predecessor) candidates)))

;;; The default output processing is valid when there is at least one
;;; input and at least one output.  We check whether either the first
;;; input and the first output are identical, or whether the first
;;; input is dead after the instruction.  In that case, we reuse the
;;; same register for the output.  Otherwise, we make sure there is an
;;; unattributed register of the right kind that we can later
;;; attribute to the output.
(defun process-outputs-default (predecessor instruction)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction)))
         (pool (output-pool instruction)))
    ;; We do not need to allocate a new register if either the output
    ;; and the input are the same or the input is dead after
    ;; INSTRUCTION.
    (if (and (typep input 'cleavir-ir:lexical-location)
             (or (eq input output)
                 (not (member input pool
                              :test #'eq :key #'lexical-location))))
        predecessor
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
    (predecessor (instruction cleavir-ir:load-constant-instruction))
  (ensure-unattributed
   predecessor instruction (first (cleavir-ir:outputs instruction))))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:argument-instruction))
  (process-outputs-default predecessor instruction))

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:initialize-return-values-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:set-return-value-instruction))
  predecessor)

(defmethod process-outputs
    (predecessor (instruction cleavir-ir:return-instruction))
  predecessor)

;;; As with PROCESS-OUTPUTS the default action for
;;; COMPUTE-OUTPUT-ARRANGEMENT is valid when there is at least one
;;; input and at least one output.
(defun compute-output-arrangement-default (instruction)
  (let* ((first-input (first (cleavir-ir:inputs instruction)))
         (first-output (first (cleavir-ir:outputs instruction)))
         (pool (output-pool instruction))
         (input-arrangement (input-arrangement instruction))
         (new-arrangement (filter-arrangement input-arrangement pool)))
    (unless (eq first-input first-output)
      ;; We must include a new register in the new arrangement.  We
      ;; know there is an unattributed register of the right kind, so
      ;; we just have to find one such register.
      (let* ((candidates (determine-candidates first-output pool))
             (register-number
               (find-unattributed-register new-arrangement candidates)))
        (reserve-register (register-map new-arrangement) register-number)
        (push (make-instance 'attribution
                :lexical-location first-output
                :stack-slot nil
                :register-number register-number)
              (attributions new-arrangement))))
    (setf (output-arrangement instruction) new-arrangement)))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:binary-operation-mixin))
  (compute-output-arrangement-default instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:assignment-instruction))
  (compute-output-arrangement-default instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memref1-instruction))
  (compute-output-arrangement-default instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memref2-instruction))
  (compute-output-arrangement-default instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:argument-instruction))
  (compute-output-arrangement-default instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:comparison-mixin))
  (setf (output-arrangement instruction)
        (filter-arrangement
         (input-arrangement instruction) (output-pool instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memset1-instruction))
  (setf (output-arrangement instruction)
        (filter-arrangement
         (input-arrangement instruction) (output-pool instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:memset2-instruction))
  (setf (output-arrangement instruction)
        (filter-arrangement
         (input-arrangement instruction) (output-pool instruction))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:initialize-return-values-instruction))
  (setf (output-arrangement instruction)
        (filter-arrangement
         (input-arrangement instruction) (output-pool instruction))))

(defun compute-output-arrangement-no-inputs (instruction)
  (let* ((input-arrangement (input-arrangement instruction))
         (pool (output-pool instruction))
         (output (first (cleavir-ir:outputs instruction)))
         (candidates (determine-candidates output pool))
         (new-register-map
           (copy-register-map (register-map input-arrangement)))
         (register-number
           (find-unattributed-register input-arrangement candidates))
         (new-attribution (make-instance 'attribution
                            :lexical-location output
                            :stack-slot nil
                            :register-number register-number)))
    (mark-register new-register-map register-number)
    (setf (output-arrangement instruction)
          (make-instance 'arrangement
            :stack-map (stack-map input-arrangement)
            :register-map new-register-map
            :attributions
            (cons new-attribution (attributions input-arrangement))))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:compute-argument-count-instruction))
  (compute-output-arrangement-no-inputs instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:load-constant-instruction))
  (compute-output-arrangement-no-inputs instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:return-instruction))
  nil)

;;; We need to make sure the output arrangement reflects the fact that
;;; only callee-saves registers contain valid objects.
(defun compute-output-arrangement-for-named-call (instruction)
  (let ((input-arrangement (input-arrangement instruction))
        (pool (output-pool instruction)))
    (with-arrangement-parts
        (stack-map register-map attributions input-arrangement)
      (let ((new-register-map (copy-register-map register-map))
            (new-attributions '()))
        (loop for attribution in attributions
              for lexical-location = (lexical-location attribution)
              for stack-slot = (stack-slot attribution)
              for register-number = (register-number attribution)
              do (when (member lexical-location pool
                               :test #'eq :key #'lexical-location)
                   ;; Then the lexical location is live.
                   (if (or (null register-number)
                           (register-number-is-callee-saves-p register-number))
                       ;; Then we keep the attribution as it is.
                       (push attribution new-attributions)
                       ;; Otherwise, we need to make sure the
                       ;; register is unattributed.
                       (progn (push (make-instance 'attribution
                                      :lexical-location lexical-location
                                      :stack-slot stack-slot
                                      :register-number nil)
                                    new-attributions)
                              (free-register
                               new-register-map register-number)))))
        (setf (output-arrangement instruction)
              (make-instance 'arrangement
                :stack-map stack-map
                :register-map new-register-map
                :attributions new-attributions))))))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:named-call-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:catch-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:bind-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:unwind-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:enclose-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:initialize-values-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defmethod compute-output-arrangement
    ((instruction cleavir-ir:multiple-value-call-instruction))
  (compute-output-arrangement-for-named-call instruction))

(defun allocate-registers-for-instructions (mir)
  (labels ((process-pair (predecessor instruction)
             (if (input-arrangement-p instruction)
                 ;; FIXME: adapt the output arrangement of PREDECESSOR
                 ;; to the existing input arrangement of INSTRUCTION.
                 nil
                 (progn (allocate-registers-for-instruction
                         predecessor instruction)
                        (loop for successor in (cleavir-ir:successors instruction)
                              do (process-pair instruction successor))))))
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
