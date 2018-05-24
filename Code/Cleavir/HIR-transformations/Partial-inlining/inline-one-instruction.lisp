(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-one-instruction :around
    (enclose-instruction
     call-instruction
     enter-instruction
     successor-instruction
     mapping)
  (declare (ignore enclose-instruction call-instruction enter-instruction mapping))
  (let ((copy (find-in-mapping *instruction-mapping* successor-instruction)))
    (if (null copy)
        (call-next-method)
        '())))

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
;;; must be created, and it must be added to the mapping, to
;;; CALL-INSTRUCTION as an input, and to ENTER-INSTRUCTION as an
;;; output.
(defun translate-output (output call-instruction enter-instruction mapping)
  (let ((new (find-in-mapping mapping output)))
    (cond ((not (null new)) new)
          ((not (local-location-p output)) output)
          ;; NOTE/FIXME?: Values locations are not made into parameters.
          ;; Therefore if inlining is stopped while a values location is live,
          ;; the HIR will not be consistent.
          ((typep output 'cleavir-ir:values-location)
           (setf new (cleavir-ir:make-values-location))
           (add-to-mapping mapping output new)
           new)
          (t (setf new (cleavir-ir:new-temporary))
             (setf (cleavir-ir:inputs call-instruction)
                   (append
                    (cleavir-ir:inputs call-instruction)
                    (list new)))
             (setf (cleavir-ir:outputs enter-instruction)
                   (append
                    (cleavir-ir:outputs enter-instruction)
                    (list new)))
             (add-to-mapping mapping output new)
             new))))
    
(defun translate-outputs (outputs call-instruction enter-instruction mapping)
  (loop for output in outputs
        collect (translate-output output
                                  call-instruction
                                  enter-instruction
                                  mapping)))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:one-successor-mixin)
     mapping)
  (let* ((inputs (cleavir-ir:inputs successor-instruction))
         (new-inputs (translate-inputs inputs mapping))
         (outputs (cleavir-ir:outputs successor-instruction))
         (new-outputs (translate-outputs outputs
                                         call-instruction
                                         enter-instruction
                                         mapping))
         (new-instruction (cleavir-ir:clone-instruction successor-instruction
                            :inputs new-inputs :outputs new-outputs
                            :predecessors nil :successors nil)))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
    (setf (cleavir-ir:successors enter-instruction)
          (cleavir-ir:successors successor-instruction))
    (list (make-instance 'worklist-item
            :enclose-instruction enclose-instruction
            :call-instruction call-instruction
            :enter-instruction enter-instruction
            :mapping mapping))))

(defmethod inline-one-instruction 
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:enclose-instruction)
     mapping)
  ;; FIXME: Just like the above, but we have to copy the CODE too.
  (let* ((inputs (cleavir-ir:inputs successor-instruction))
         (new-inputs (translate-inputs inputs mapping))
         (outputs (cleavir-ir:outputs successor-instruction))
         (new-outputs (translate-outputs outputs
                                         call-instruction
                                         enter-instruction
                                         mapping))
         (code (copy-function (cleavir-ir:code successor-instruction) mapping))
         (new-instruction (cleavir-ir:clone-instruction successor-instruction
                            :inputs new-inputs :outputs new-outputs
                            :predecessors nil :successors nil
                            :code code)))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
    (setf (cleavir-ir:successors enter-instruction)
          (cleavir-ir:successors successor-instruction))
    (list (make-instance 'worklist-item
            :enclose-instruction enclose-instruction
            :call-instruction call-instruction
            :enter-instruction enter-instruction
            :mapping mapping))))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:unwind-instruction)
     mapping)
  (let ((destination (cleavir-ir:destination successor-instruction)))
    (if (local-catch-p destination)
        ;; We are unwinding to the function being inlined into,
        ;; so the UNWIND-INSTRUCTION must be reduced to a NOP.
        (let ((new-instruction (cleavir-ir:make-nop-instruction nil)))
          (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
          (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
          (setf (cleavir-ir:successors enter-instruction)
                (list (second (cleavir-ir:successors destination))))
          (list (make-instance 'worklist-item
                  :enclose-instruction enclose-instruction
                  :call-instruction call-instruction
                  :enter-instruction enter-instruction
                  :mapping mapping)))
        ;; We are still actually unwinding, but need to ensure the
        ;; DESTINATION is hooked up correctly.
        (let* ((inputs (cleavir-ir:inputs successor-instruction))
               (new-inputs (translate-inputs inputs mapping))
               (outputs (cleavir-ir:outputs successor-instruction))
               (new-outputs (translate-outputs outputs
                                               call-instruction
                                               enter-instruction
                                               mapping))
               (destination (or (find-in-mapping *instruction-mapping* destination)
                                destination))
               (new-instruction (cleavir-ir:clone-instruction successor-instruction
                                  :inputs new-inputs :outputs new-outputs
                                  :predecessors nil :successors nil
                                  :destination destination)))
          (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
          (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
          (setf (cleavir-ir:successors enter-instruction)
                (cleavir-ir:successors successor-instruction))
          '()))))

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
  (let* ((inputs (cleavir-ir:inputs successor-instruction))
         (new-inputs (translate-inputs inputs mapping))
         (outputs (cleavir-ir:outputs successor-instruction))
         (new-outputs (translate-outputs outputs
                                         call-instruction
                                         enter-instruction
                                         mapping))
         (new-instruction (cleavir-ir:clone-instruction successor-instruction
                            :inputs new-inputs
                            :outputs new-outputs
                            :predecessors nil :successors nil))
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
    (list (make-instance 'worklist-item
            :enclose-instruction enclose-instruction
            :call-instruction call-instruction
            :enter-instruction enter-instruction
            :mapping mapping)
          (make-instance 'worklist-item
            :enclose-instruction new-enclose
            :call-instruction new-call
            :enter-instruction new-enter
            :mapping (copy-mapping mapping)))))

(defmethod inline-one-instruction
    (enclose-instruction
     call-instruction
     enter-instruction
     (successor-instruction cleavir-ir:return-instruction)
     mapping)
  ;; If we get as far as the return, we are fully inlining.
  ;; As such, we don't need to keep the call or enclose, and just go around them.
  ;; set-predecessors can disconnect them later.
  (let ((new-instruction (let ((cleavir-ir:*policy* (cleavir-ir:policy successor-instruction)))
                           (cleavir-ir:make-nop-instruction nil)))
        (call-successor (first (cleavir-ir:successors call-instruction))))
    (add-to-mapping *instruction-mapping* successor-instruction new-instruction)
    (cleavir-ir:insert-instruction-before new-instruction enclose-instruction)
    ;; manual hookup
    (setf (cleavir-ir:successors new-instruction) (list call-successor))
    (setf (cleavir-ir:predecessors call-successor)
          (substitute new-instruction call-instruction (cleavir-ir:predecessors call-successor)))
    '()))
