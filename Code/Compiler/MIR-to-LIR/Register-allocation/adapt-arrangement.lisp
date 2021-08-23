(in-package #:sicl-register-allocation)

;;; An instruction which is a placeholder for replacement later into
;;; MIR-to-LIR.  A later pass after registers are introduced will
;;; replace an ADAPT-INSTRUCTION with instructions to adapt between
;;; arrangements.
;;;
;;; This instruction has no inputs or outputs, and has only one
;;; predecessor and one successor.
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

;;; A class which just makes it easier to find instructions generated
;;; by adaptation.
(defclass adapt-assignment-instruction (cleavir-ir:assignment-instruction)
  ())

;;; First, we make a list of parallel assignments which need to be
;;; performed to adapt between arrangements.  Each location is
;;; designated by a list like (stack-slot register-number), where
;;; either element is either NIL or a non-negative integer.
(defun identify-assignments (target-arrangement source-arrangement)
  (let ((assignments (make-hash-table :test 'equal)))
    (arr:map-attributions
     (lambda (location target-stack target-register)
       (multiple-value-bind (source-stack source-register)
           (arr:find-attribution source-arrangement location)
         (assert (not (and (null source-stack)
                           (null source-register))))
         (unless (and (eql target-stack source-stack)
                      (eql target-register source-register))
           (setf (gethash (list target-stack target-register)
                          assignments)
                 (list source-stack source-register)))))
     target-arrangement)
    assignments))

(defstruct assignment-chain
  (assignments '()))

;;; For some parallel assignments like B <- A; A <- B, we generate the
;;; sequential assignments C <- A; A <- B; B <- C.
(defun spill-chain (chain location source)
  (setf (assignment-chain-assignments chain)
        (append (list (list 'spill source))
                (assignment-chain-assignments chain)
                (list (list location 'spill)))))

;;; The new algorithm for adapting arrangements identifies "chains" of
;;; assignments which can be performed sequentially.  If a loop is
;;; found, then it is broken by emitting assignments to a virtual
;;; SPILL location, which is replaced with a real location when we
;;; generate code for the adaptation.
(defun identify-chains (assignments)
  (let ((chain-for-location (make-hash-table :test 'equal))
        (all-chains         (make-hash-table :test 'eq))
        (locations-to-check (alexandria:hash-table-keys assignments)))
    (flet ((traverse-chain (first-location)
             ;; Build up a chain by traversing the sources of each
             ;; assignment.  We either create a new chain or join onto
             ;; an already existing chain.
             (loop with chain = (make-assignment-chain)
                   for location = first-location then source
                   for source = (gethash location assignments)
                   for existing-chain = (gethash source chain-for-location)
                   do (alexandria:removef locations-to-check location
                                          :test #'equal
                                          :count 1)
                   when (null source)
                     do (return chain)
                   unless (null existing-chain)
                     do (cond
                          ;; If a location was already assigned in
                          ;; this chain, then we have discovered a
                          ;; loop.
                          ((eq existing-chain chain)
                           (spill-chain chain location source))
                          ;; Otherwise, we found part of a chain that
                          ;; was not traversed before.  Note that
                          ;; joining chain cannot possibly form a
                          ;; loop; as we would have traversed this
                          ;; part before if the chain looped.
                          (t
                           (setf (assignment-chain-assignments existing-chain)
                                 (append (assignment-chain-assignments chain)
                                         (assignment-chain-assignments existing-chain)))
                           ;; Replace all uses of this chain with
                           ;; the already existing chain.
                           (maphash (lambda (source other-chain)
                                      (when (eq chain other-chain)
                                        (setf (gethash source chain-for-location)
                                              existing-chain)))
                                    chain-for-location)))
                        (return existing-chain)
                   do (push (list location source)
                            (assignment-chain-assignments chain))
                      (setf (gethash location chain-for-location) chain
                            (gethash chain all-chains) t))))
      (loop until (null locations-to-check)
            do (traverse-chain (pop locations-to-check)))
      (alexandria:hash-table-keys all-chains))))

;;; A totally arbitrary choice of register, used temporarily to
;;; transfer values from the stack back to the stack, should there be no
;;; other free registers.
(defvar *stack-copy-register* (x86-64:register-number x86-64:*r15*))

(defun first-free-stack-slot (&rest arrangements)
  (reduce #'max arrangements :key #'arr:first-stack-slot-past-arrangement))

(defun temporary-locations (target source)
  (let* ((free-registers      (arr:free-registers target source))
         (first-stack-slot    (first-free-stack-slot target source))
         (first-free-register (x86-64:find-any-register-in-map free-registers)))
    (if (null first-free-register)
        ;; If there are no free registers, pick one at random for
        ;; stack-stack assignments, and a stack slot for spilling.
        (values *stack-copy-register*
                first-stack-slot
                (list (1+ first-stack-slot) nil))
        (let ((second-free-register
                (x86-64:find-any-register-in-map free-registers
                                                 :start first-free-register)))
          (values first-free-register
                  first-stack-slot
                  (if (null second-free-register)
                      (list (1+ first-stack-slot) nil)
                      (list nil second-free-register)))))))

;;; Generate adaptation code based on assignment chains. We may
;;; redirect uses of the stack copy register number COPY-REGISTER to a
;;; stack slot COPY-STACK-SLOT, and use the SPILL-LOCATION to break
;;; assignment cycles (which were identified with the previously
;;; mentioned SPILL location) by spilling a register.
(defun generate-adaptation-code (chains predecessor successor
                                 copy-register copy-stack-slot spill-location)
  (sicl-utilities:with-collectors ((instructions add-instruction))
    (let ((used-copy-register-p nil)
          (performed-stack-stack-copy-p nil))
      (labels ((register (number)
                 (aref x86-64:*registers* number))
               (%emit (target source)
                 (destructuring-bind ((target-stack target-register)
                                      (source-stack source-register))
                     (list target source)
                   (cond
                     ((and (null target-register) (null source-register))
                      (setf performed-stack-stack-copy-p t)
                      (add-instruction
                       (x86-64:load-from-stack-instruction
                        source-stack (register copy-register)))
                      (add-instruction
                       (x86-64:save-to-stack-instruction
                        (register copy-register) target-stack)))
                     ((null source-register)
                      (add-instruction
                       (x86-64:load-from-stack-instruction
                        source-stack (register target-register))))
                     ((null target-register)
                      (add-instruction
                       (x86-64:save-to-stack-instruction
                        (register source-register) target-stack)))
                     (t
                      (add-instruction
                       (make-instance 'adapt-assignment-instruction
                         :inputs (list (register source-register))
                         :outputs (list (register target-register))))))))
               (rewrite-spill (location)
                 (cond
                   ((eql location 'spill)
                    spill-location)
                   ((equal location (list nil copy-register))
                    (setf used-copy-register-p t)
                    (list copy-stack-slot nil))
                   (t
                    location)))
               (emit (target source)
                 (%emit (rewrite-spill target) (rewrite-spill source))))
        (dolist (chain chains)
          (loop for (target source) in (assignment-chain-assignments chain)
                do (emit target source)))
        (flet ((add-instruction (instruction)
                 (mark-as-generated instruction)
                 (cleavir-ir:insert-instruction-between
                  instruction
                  predecessor successor)
                 (setf predecessor instruction)))
          (cond
            ((and used-copy-register-p performed-stack-stack-copy-p)
             ;; If we used the copy register and actually performed a
             ;; stack to stack copy, we need to spill the copy register
             ;; to the stack before any adaptation, and then unspill
             ;; after adaptation.
             (add-instruction
              (x86-64:save-to-stack-instruction
               (register copy-register) copy-stack-slot))
             (mapc #'add-instruction (instructions))
             (add-instruction
              (x86-64:load-from-stack-instruction
               copy-stack-slot (register copy-register))))
            (t
             (mapc #'add-instruction (instructions)))))))))

(defmethod introduce-registers-for-instruction ((instruction adapt-instruction))
  (destructuring-bind (successor)
      (cleavir-ir:successors instruction)
    (let ((target (output-arrangement instruction))
          (source (input-arrangement instruction)))
      (multiple-value-bind (copy-register copy-stack spill-location)
          (temporary-locations target source)
        (generate-adaptation-code
         (identify-chains
          (identify-assignments target source))
         instruction successor
         copy-register copy-stack spill-location))))
  (cleavir-ir:delete-instruction instruction))
