(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-one-instruction :around
    (enclose-instruction
     call-instruction
     enter-instruction
     successor-instruction
     mapping)
  (declare (ignore enter-instruction mapping))
  (let ((copy (find-in-mapping *instruction-mapping* successor-instruction)))
    (cond ((null copy)
           (call-next-method))
          (t
           ;; In this case, we have reached an instruction that has already
           ;; been copied. Most likely, we branched earlier and have now
           ;; hit the join. As such, we can eliminate our residual function.
           ;; We do so by bypassing the enclose and funcall; we do the same
           ;; thing when we hit a RETURN-INSTRUCTION (below)
           (loop for pred in (cleavir-ir:predecessors enclose-instruction)
                 do (setf (cleavir-ir:successors pred)
                          (substitute copy enclose-instruction
                                      (cleavir-ir:successors pred))
                          (cleavir-ir:predecessors copy)
                          (substitute pred call-instruction
                                      (cleavir-ir:predecessors copy))))
           ;; Our work here is done: return no worklist items.
           '()))))

(defun local-catch-p (instruction)
  (eq (instruction-owner instruction) *target-enter-instruction*))

(defun local-location-p (location)
  (eq (location-owner location) *original-enter-instruction*))

(defun translate-input (input mapping)
  ;; An input is either already in the mapping, or else it is
  ;; is a location that is owned by some ancestor function.
  (or (find-in-mapping mapping input) input))

(defun translate-inputs (inputs mapping)
  (loop for input in inputs
        collecting (translate-input input mapping)))

;;; An output can either be in the mapping, be a reference to a
;;; location owned by an ancestor function, or a local lexical
;;; location not yet in the mapping.  In the last case, a new location
;;; must be created, and it must be added to the mapping.
(defun translate-output (output mapping)
  (let ((new (find-in-mapping mapping output)))
    (cond ((not (null new)) new)
          ((not (local-location-p output)) output)
          ;; NOTE/FIXME?: Values locations are not made into parameters.
          ;; Therefore if inlining is stopped while a values location is live,
          ;; the HIR will not be consistent.
          ((typep output 'cleavir-ir:values-location)
           (setf new (cleavir-ir:make-values-location))
           (add-to-mapping mapping output new)
           (setf (location-owner new) *target-enter-instruction*)
           new)
          (t (setf new (cleavir-ir:make-lexical-location
                        (cleavir-ir:name output)))
             (add-to-mapping mapping output new)
             (setf (location-owner new) *target-enter-instruction*)
             new))))
    
(defun translate-outputs (outputs mapping)
  (loop for output in outputs
        collect (translate-output output mapping)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COPYING INSTRUCTIONS

;;; Set up the ownership of the new instruction.
(defmethod copy-instruction :around (instruction mapping)
  (let ((result (call-next-method)))
    (setf (instruction-owner result) *target-enter-instruction*)
    result))

(defmethod copy-instruction (instruction mapping)
  (cleavir-ir:clone-instruction instruction
    :inputs (translate-inputs (cleavir-ir:inputs instruction) mapping)
    :outputs (translate-outputs (cleavir-ir:outputs instruction) mapping)
    :predecessors nil :successors nil
    :dynamic-environment (translate-input
                          (cleavir-ir:dynamic-environment instruction) mapping)))

(defmethod copy-instruction ((instruction cleavir-ir:enclose-instruction) mapping)
  (cleavir-ir:clone-instruction instruction
    :inputs (translate-inputs (cleavir-ir:inputs instruction) mapping)
    :outputs (translate-outputs (cleavir-ir:outputs instruction) mapping)
    :predecessors nil :successors nil
    :dynamic-environment (translate-input
                          (cleavir-ir:dynamic-environment instruction) mapping)
    :code (copy-function (cleavir-ir:code instruction) mapping)))

(defmethod copy-instruction ((instruction cleavir-ir:unwind-instruction) mapping)
  (let ((destination (cleavir-ir:destination instruction)))
    (if (local-catch-p destination)
        ;; We are unwinding to the function being inlined into,
        ;; so the UNWIND-INSTRUCTION must be reduced to a NOP.
        (let ((target (nth (cleavir-ir:unwind-index instruction)
                           (cleavir-ir:successors destination)))
              (cleavir-ir:*policy* (cleavir-ir:policy instruction))
              (cleavir-ir:*dynamic-environment*
                (translate-input (cleavir-ir:dynamic-environment instruction)
                                 mapping)))
          (cleavir-ir:make-nop-instruction (list target)))
        ;; We are still actually unwinding, but need to ensure the
        ;; DESTINATION is hooked up correctly.
        (let* ((inputs (cleavir-ir:inputs instruction))
               (new-inputs (translate-inputs inputs mapping))
               (destination (or (find-in-mapping *instruction-mapping* destination)
                                destination)))
          (cleavir-ir:clone-instruction instruction
            :inputs new-inputs :outputs nil
            :predecessors nil :successors nil
            :dynamic-environment (translate-input
                                  (cleavir-ir:dynamic-environment instruction) mapping)
            :destination destination)))))

;;; See INLINE-ONE-INSTRUCTION method, below.
(defmethod copy-instruction ((instruction cleavir-ir:return-instruction) mapping)
  (let ((cleavir-ir:*policy* (cleavir-ir:policy instruction))
        (cleavir-ir:*dynamic-environment*
          (translate-input
           (cleavir-ir:dynamic-environment instruction) mapping)))
    (cleavir-ir:make-nop-instruction nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ONE SUCCESSOR

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:one-successor-mixin)
     mapping)
  (let ((new-instruction (copy-instruction successor-instruction mapping)))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
    (setf (cleavir-ir:successors enter-instruction)
          (cleavir-ir:successors successor-instruction))
    (list (make-instance 'worklist-item
            :enclose-instruction enclose-instruction
            :call-instruction call-instruction
            :enter-instruction enter-instruction
            :mapping mapping))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TWO SUCCESSORS

(defun add-multiple-successor-instruction-before-instruction
    (instruction-to-add
     before-instruction
     new-successors)
  ;; Start by setting the predecessors of the instruction to add to be
  ;; the current predecessors of the instruction before which the
  ;; new instruction is to be added.
  (setf (cleavir-ir:predecessors instruction-to-add)
        (cleavir-ir:predecessors before-instruction))
  ;; It is possible that BEFORE-INSTRUCTION has more than one
  ;; predecessor.  And it is possible that each one of those
  ;; predecessors has more than one successor. So for each such
  ;; predecessor, replace BEFORE-INSTRUCTION by INSTRUCTION-TO-ADD in
  ;; its list of successors.
  (loop for predecessor in (cleavir-ir:predecessors before-instruction)
        do (setf (cleavir-ir:successors predecessor)
                 (substitute instruction-to-add
                             before-instruction
                             (cleavir-ir:successors predecessor))))
  ;; Set the successors of the instruction to add to be the list of
  ;; the successors we were given as an argument.
  (setf (cleavir-ir:successors instruction-to-add) new-successors)
  ;; Each of the successors we were given as arguments will now
  ;; have a single predecessor, namely INSTRUCTION-TO-ADD, so we set
  ;; the list of predecessors of each one to a singleton list of
  ;; INSTRUCTION-TO-ADD.
  (loop for successor in new-successors
        do (setf (cleavir-ir:predecessors successor)
                 (list instruction-to-add))))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:multiple-successors-mixin)
     mapping)
  (let* ((new-instruction (copy-instruction successor-instruction mapping))
         (prime-successor (first (cleavir-ir:successors successor-instruction)))
         ;; For each successor but the first, we need a new enclose/call
         ;; sequence.
         (alt-successors (rest (cleavir-ir:successors successor-instruction)))
         (new-closures (loop for succ in alt-successors
                             collect (cleavir-ir:new-temporary)))
         (new-calls (loop with cleavir-ir:*policy* = (cleavir-ir:policy call-instruction)
                          with cleavir-ir:*dynamic-environment*
                            = (cleavir-ir:dynamic-environment call-instruction)
                          for succ in alt-successors
                          for new-closure in new-closures
                          collect
                          ;; The new CALL-INSTRUCTION is like the previous one, except that
                          ;; the first input is the new closure to be called.
                          (cleavir-ir:make-funcall-instruction
                           (cons new-closure (rest (cleavir-ir:inputs call-instruction)))
                           (cleavir-ir:outputs call-instruction)
                           (first (cleavir-ir:successors call-instruction)))))
         (new-enters (loop with cleavir-ir:*policy* = (cleavir-ir:policy enter-instruction)
                           with cleavir-ir:*dynamic-environment*
                             = (cleavir-ir:dynamic-environment enter-instruction)
                           for succ in alt-successors
                           collect
                           (cleavir-ir:make-enter-instruction
                            (cleavir-ir:lambda-list enter-instruction)
                            cleavir-ir:*dynamic-environment*
                            :successor succ
                            :origin (cleavir-ir:origin enter-instruction))))
         (new-encloses (loop with cleavir-ir:*policy* = (cleavir-ir:policy enclose-instruction)
                             with cleavir-ir:*dynamic-environment*
                               = (cleavir-ir:dynamic-environment enclose-instruction)
                             for succ in alt-successors
                             for closure in new-closures
                             for call in new-calls
                             for enter in new-enters
                             collect
                             (cleavir-ir:make-enclose-instruction closure call enter))))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (add-multiple-successor-instruction-before-instruction
     new-instruction enclose-instruction (list* enclose-instruction new-encloses))
    ;; For the first successor, we just reuse the enter and enclose and such.
    (setf (cleavir-ir:successors enter-instruction) (list prime-successor))
    ;; Now just the worklist.
    (list* (make-instance 'worklist-item
             :enclose-instruction enclose-instruction
             :call-instruction call-instruction
             :enter-instruction enter-instruction
             :mapping mapping)
           (loop for new-enclose in new-encloses
                 for new-call in new-calls
                 for new-enter in new-enters
                 collect (make-instance 'worklist-item
                           :enclose-instruction new-enclose
                           :call-instruction new-call
                           :enter-instruction new-enter
                           :mapping mapping)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NO SUCCESSORS
;;;
;;; All of these result in no more work, as the block is finished. The residual
;;; function, i.e. the enclose and funcall, are discarded.

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:no-successors-mixin)
     mapping)
  (let ((new-instruction (copy-instruction successor-instruction mapping)))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:bypass-instruction new-instruction enclose-instruction)
    '()))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:return-instruction)
     mapping)
  (let* ((call-successor (first (cleavir-ir:successors call-instruction)))
         (new-instruction (copy-instruction successor-instruction mapping)))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:bypass-instruction new-instruction enclose-instruction)
    ;; manual hookup
    (setf (cleavir-ir:successors new-instruction)
          (list call-successor)
          (cleavir-ir:predecessors call-successor)
          (substitute new-instruction call-instruction
                      (cleavir-ir:predecessors call-successor)))
    '()))
