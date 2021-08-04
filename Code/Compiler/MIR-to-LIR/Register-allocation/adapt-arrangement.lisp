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

;;; A totally arbitrary choice of register, used temporarily to
;;; transfer values from the stack back to the stack, should there be no
;;; other free registers.
(defvar *stack-copy-register* x86-64:*r15*)

(defun first-free-stack-slot (&rest arrangements)
  (reduce #'max arrangements :key #'arr:first-stack-slot-past-arrangement))

;;; A class which just makes it easier to find instructions generated
;;; by adaptation.
(defclass adapt-assignment-instruction (cleavir-ir:assignment-instruction)
  ())

;;; First, we make a list of parallel assignments which need to be
;;; performed to adapt between arrangements.
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
  (let ((chains (make-hash-table :test 'equal))
        (locations-to-check (alexandria:hash-table-keys assignments)))
    (flet ((traverse-chain (first-location)
             ;; Build up a chain by traversing the sources of each
             ;; assignment.  We either create a new chain or join onto
             ;; an already existing chain.
             (loop with chain = (make-assignment-chain)
                   for location = first-location then source
                   for source = (gethash location assignments)
                   for existing-chain = (gethash source chains)
                   do (alexandria:removef locations-to-check location
                                          :test #'equal)
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
                          ;; joining chains cannot possibly form a
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
                                        (setf (gethash source chains)
                                              existing-chain)))
                                    chains)))
                        (return existing-chain)
                   do (push (list location source)
                            (assignment-chain-assignments chain))
                      (setf (gethash location chains) chain))))
      (loop until (null locations-to-check)
            do (traverse-chain (pop locations-to-check)))
      (remove-duplicates
       (alexandria:hash-table-values chains)))))



(defmethod introduce-registers-for-instruction ((instruction adapt-instruction))
  (destructuring-bind (successor)
      (cleavir-ir:successors instruction)
    (adapt-arrangements-between-instructions instruction successor
                                             (input-arrangement instruction)
                                             (output-arrangement instruction))
    (cleavir-ir:delete-instruction instruction)))
