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
  (eq (gethash instruction *instruction-ownerships*)
      *target-enter-instruction*))

(defun local-location-p (location)
  (eq (gethash location *location-ownerships*)
      *original-enter-instruction*))

(defun translate-inputs (inputs mapping)
  ;; An input is either already in the mapping, or else it is
  ;; is a location that is owned by some ancestor function.
  (loop for input in inputs
        for new = (find-in-mapping mapping input)
        collect (if (null new) input new)))

;;; An output can either be in the mapping, be a reference to a
;;; location owned by an ancestor function, or a local lexical
;;; location not yet in the mapping.  In the last case, a new location
;;; must be created, and it must be added to the mapping.
(defun translate-output (output mapping)
  (let ((new (find-in-mapping mapping output)))
    (cond ((not (null new)) new)
          ((not *copy-locations*) output)
          ((not (local-location-p output)) output)
          ;; NOTE/FIXME?: Values locations are not made into parameters.
          ;; Therefore if inlining is stopped while a values location is live,
          ;; the HIR will not be consistent.
          ((typep output 'cleavir-ir:values-location)
           (setf new (cleavir-ir:make-values-location))
           (add-to-mapping mapping output new)
           (setf (gethash new *location-ownerships*) *target-enter-instruction*)
           new)
          (t (setf new (cleavir-ir:make-lexical-location
                        (cleavir-ir:name output)))
             (setf (gethash new *location-ownerships*) *target-enter-instruction*)
             (add-to-mapping mapping output new)
             new))))
    
(defun translate-outputs (outputs mapping)
  (loop for output in outputs
        collect (translate-output output mapping)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COPYING INSTRUCTIONS

(defmethod copy-instruction (instruction mapping)
  (cleavir-ir:clone-instruction instruction
    :inputs (translate-inputs (cleavir-ir:inputs instruction) mapping)
    :outputs (translate-outputs (cleavir-ir:outputs instruction) mapping)
    :predecessors nil :successors nil))

;;; Set up the new instruction's owner as the function we're inlining into.
;;; NOTE: For a partial inline, it should be the enter in the worklist instead.
;;; But we don't do actual partial inlining yet.
(defmethod copy-instruction :around (instruction mapping)
  (declare (ignore mapping))
  (let ((result (call-next-method)))
    (setf (gethash result *instruction-ownerships*) *target-enter-instruction*)
    result))

(defmethod copy-instruction ((instruction cleavir-ir:enclose-instruction) mapping)
  (if *copy-functions*
      (cleavir-ir:clone-instruction instruction
        :inputs (translate-inputs (cleavir-ir:inputs instruction) mapping)
        :outputs (translate-outputs (cleavir-ir:outputs instruction) mapping)
        :predecessors nil :successors nil
        :code (copy-function (cleavir-ir:code instruction) mapping))
      (call-next-method)))

(defmethod copy-instruction :around ((instruction cleavir-ir:catch-instruction) mapping)
  ;; If we are NOT copying subfunctions, we need to ensure that any lower
  ;; UNWINDs refer to our new catch instruction and not the original.
  ;; We do so by editing the enclosed function.
  (let ((new-instruction (call-next-method)))
    (unless *copy-functions*
      ;; We work out what has the CATCH as a destination by checking where its output is used.
      (loop for unwind in (cleavir-ir:using-instructions (first (cleavir-ir:outputs instruction)))
            ;; as of now, all uses should be UNWINDs,
            ;; but i don't think we ought to make that required, therefore...
            when (typep unwind 'cleavir-ir:unwind-instruction)
              do (setf (cleavir-ir:destination unwind) new-instruction)))
    new-instruction))

(defmethod copy-instruction ((instruction cleavir-ir:unwind-instruction) mapping)
  (let ((destination (cleavir-ir:destination instruction)))
    (if (local-catch-p destination)
        ;; We are unwinding to the function being inlined into,
        ;; so the UNWIND-INSTRUCTION must be reduced to a NOP.
        (let ((target (second (cleavir-ir:successors destination)))
              (cleavir-ir:*policy* (cleavir-ir:policy instruction)))
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
            :destination destination)))))

(defmethod copy-instruction ((instruction cleavir-ir:return-instruction) mapping)
  (declare (ignore mapping))
  (let ((cleavir-ir:*policy* (cleavir-ir:policy instruction)))
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

(defun add-two-successor-instruction-before-instruction
    (instruction-to-add
     before-instruction
     new-first-successor
     new-second-successor)
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
  ;; the two successors we were given as arguments.
  (setf (cleavir-ir:successors instruction-to-add)
        (list new-first-successor new-second-successor))
  ;; Each of the two successors we were given as arguments will now
  ;; have a single predecessor, namely INSTRUCTION-TO-ADD, so we set
  ;; the list of predecessors of each one to a singleton list of
  ;; INSTRUCTION-TO-ADD.
  (setf (cleavir-ir:predecessors new-first-successor)
        (list instruction-to-add))
  (setf (cleavir-ir:predecessors new-second-successor)
        (list instruction-to-add)))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:two-successors-mixin)
     mapping)
  (let* ((new-instruction (copy-instruction successor-instruction mapping))
         ;; Create a temporary to hold the closure to be called by the
         ;; call-instruction in the second branch of the new
         ;; instruction.
         (new-temp (cleavir-ir:new-temporary))
         ;; The new CALL-INSTRUCTION is like the previous one, except that
         ;; the first input is the new closure to be called.
         (new-call (let ((cleavir-ir:*policy* (cleavir-ir:policy call-instruction)))
                     (cleavir-ir:make-funcall-instruction
                      (cons new-temp (rest (cleavir-ir:inputs call-instruction)))
                      (cleavir-ir:outputs call-instruction)
                      (first (cleavir-ir:successors call-instruction)))))
         (new-enter (let ((cleavir-ir:*policy* (cleavir-ir:policy enter-instruction)))
                      (cleavir-ir:make-enter-instruction
                       (cleavir-ir:lambda-list enter-instruction)
                       :successor (second (cleavir-ir:successors successor-instruction))
                       :origin (cleavir-ir:origin enter-instruction))))
         (new-enclose (let ((cleavir-ir:*policy* (cleavir-ir:policy enclose-instruction)))
                        (cleavir-ir:make-enclose-instruction
                         new-temp new-call new-enter))))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (add-two-successor-instruction-before-instruction
     new-instruction enclose-instruction enclose-instruction new-enclose)
    (setf (cleavir-ir:successors enter-instruction)
          (list (first (cleavir-ir:successors successor-instruction))))
    (setf (cleavir-ir:successors new-enter)
          (list (second (cleavir-ir:successors successor-instruction))))
    (setf (gethash new-call *instruction-ownerships*) *target-enter-instruction*
          (gethash new-enclose *instruction-ownerships*) *target-enter-instruction*
          (gethash new-enter *instruction-ownerships*) new-enter
          (gethash new-temp *location-ownerships*) *target-enter-instruction*)
    (list (make-instance 'worklist-item
            :enclose-instruction enclose-instruction
            :call-instruction call-instruction
            :enter-instruction enter-instruction
            :mapping mapping)
          (make-instance 'worklist-item
            :enclose-instruction new-enclose
            :call-instruction new-call
            :enter-instruction new-enter
            :mapping mapping))))

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
