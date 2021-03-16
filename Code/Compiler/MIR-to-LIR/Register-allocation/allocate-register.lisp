(cl:in-package #:sicl-register-allocation)

;;; Find the first unassigned register.  If all registers are
;;; assigned, return NIL.
(defun find-unassigned-register (arrangement candidates)
  (loop for candidate in candidates
        unless (member candidate (attributions arrangement)
                       :test #'eq :key #'register)
          return candidate
        finally (return nil)))

(defun extract-locations (arrangement candidates)
  (loop for candidate in candidates
        for attribution = (find candidate (attributions arrangement)
                                :test #'eq :key #'register)
        collect (lexical-location attribution)))

(defun extract-pool-items (pool locations)
  (loop for location in locations
        collect (find location pool :test #'eq :key #'lexical-location)))

(defun allocate-register
    (predecessor instruction pool arrangement candidates)
  (let ((unassigned (find-unassigned-register arrangement candidates)))
    (if (not (null unassigned))
        unassigned
        (let* ((locations (extract-locations arrangement candidates))
               (pool-items (extract-pool-items pool locations))
               (sorted-pool-items (sort pool-items #'> :key #'distance))
               (location (lexical-location (first sorted-pool-items)))
               (attribution (find location (attributions arrangement)
                                  :test #'eq :key #'lexical-location))
               (register (register attribution)))
          (values register
                  (if (null (stack-slot attribution))
                      (spill predecessor instruction register)
                      predecessor))))))

;;; Return the register that is attributed to LEXICAL-LOCATION in
;;; ARRANGEMENT, or NIL if no register is atributed to
;;; LEXICAL-LOCATION.  We know that LEXICAL-LOCATION is live, so we
;;; know that it has an attribution in ASSIGNEMENT.  We just don't
;;; know whether the REGISTER of that attribution is a register or
;;; NIL.
(defun attributed-register (lexical-location arrangement)
  (register (find lexical-location (attributions arrangement)
                  :key #'lexical-location :test #'eq)))

;;; Split the lexical locations in USED into two groups retuned as two
;;; values.  The first group constists of lexical locations that
;;; should be allocated in caller-saves registers and the second group
;;; consists of lexical locations that should be allocated in
;;; callee-saves registers.
(defun split-locations (used pool)
  (loop for location in used
        for pool-item = (find location pool
                              :key #'lexical-location :test #'eq)
        if (or
            ;; The location is dead.
            (null pool-item)
            ;; It has very little chance of being used across
            ;; a function call.
            (<= (call-probability pool-item) 2))
          collect location into caller-saves
        else
          collect location into callee-saves
        finally (return (values caller-saves callee-saves))))

;;; From ATTRIBUTIONS, extract attributions with a register that is an
;;; element of REGISTERS.
(defun extract-attributions (attributions registers)
  (loop for attribution in attributions
        when (member (register attribution) registers)
          collect attribution))

;;; If there is at least one register in REGISTERS that is not in any
;;; attribution in ATTRIBUTIONS, then return any such register.
;;; Otherwise return NIL.
(defun find-unattributed-register (attributions registers)
  (loop for register in registers
        for attribution = (find register attributions
                                :key #'register :test #'eq)
        when (null attribution)
          return register
        finally (return nil)))

(defun ensure-registers
    (predecessor instruction pool used created)
  (let* ((arrangement (output-arrangement predecessor))
         (stack-map (stack-map arrangement))
         (attributions (attributions arrangement))
         (new-attributions attributions)
         ;; Extract attributions that have a caller-saves register.
         (caller-saves-attributions
           (extract-attributions attributions *caller-saves*))
         ;; Extract attributions that have a callee-saves register.
         (callee-saves-attributions
           (extract-attributions attributions *callee-saves*))
         (remaining-used used)
         (protected '()))
    ;; First check whether there are any locations in the USED list
    ;; that have an attribution in the arragment that has a register,
    ;; meaning that the location is already in a register.  If so,
    ;; protect those arrangements, so that they won't be altered by
    ;; the following code.  Also, remove those locations from
    ;; REMAINING-USED, becaue they then need no further consideration.
    (loop for location in used
          for attribution = (find location (attributions arrangement)
                                  :key #'lexical-location :test #'eq)
          unless (null attribution)
            do (let ((register (register attribution)))
                 (unless (null register)
                   (setf remaining-used
                         (remove location remaining-used :test #'eq))
                   (push attribution protected))))
    ;; We are left with locations in REMAINING-USED that currently do
    ;; not have a register, but that need one.  We start by splitting
    ;; them up into two categories, namely the ones that need
    ;; CALLER-SAVES registers, and the ones that need CALLEE-SAVES
    ;; registers.
    (multiple-value-bind (caller-saves-locations callee-saves-locations)
        (split-locations remaining-used pool)
      ;; Next, we handle all the locations for which there is an
      ;; unattributed register, because those locations don't need any
      ;; additional instructions for spilling or unspilling.
      (let ((remaining-caller caller-saves-locations)
            (remaining-callee callee-saves-locations))
        ;; FIXME: create an abstraction here.  There is way too much
        ;; duplicated code.
        (loop for location in caller-saves-locations
              for register
                = (find-unattributed-register
                   caller-saves-attributions *caller-saves*)
              unless (null register)
                do (setf remaining-caller
                         (remove location remaining-caller :test #'eq))
                   (push (make-instance 'attribution
                           :lexical-location location
                           :register register)
                         new-attributions))
        (loop for location in callee-saves-locations
              for register
                = (find-unattributed-register
                   callee-saves-attributions *callee-saves*)
              unless (null register)
                do (setf remaining-callee
                         (remove location remaining-callee :test #'eq))
                   (push (make-instance 'attribution
                           :lexical-location location
                           :register register)
                         new-attributions))
        ;; At this point, we have done anything we can in terms of the
        ;; USED locations that does not require any aditional spill or
        ;; unspill.  In NEW-ATTRIBUTIONS we have attributions that are
        ;; valid at the input of any new instruction required for
        ;; spilling or unspilling, and in STACK-MAP we have the
        ;; unaltered stack map of the output arrangement of
        ;; PREDECESSOR.  So what we do now is we insert a NOP
        ;; instruction to hold the output arrangement of PREDECESSOR
        ;; as its input arrangement, and the modified arrangement as
        ;; its output arrangement.
        (let ((nop-instruction (make-instance 'cleavir-ir:nop-instruction)))
          (cleavir-ir:insert-instruction-between
           nop-instruction predecessor instruction)
          (setf (input-arrangement nop-instruction) arrangement)
          (setf (output-arrangement nop-instruction)
                (make-instance 'arrangement
                  :stack-map stack-map
                  :attributions new-attributions)))))))
