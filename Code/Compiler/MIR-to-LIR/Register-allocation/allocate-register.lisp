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
