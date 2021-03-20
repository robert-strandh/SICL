(cl:in-package #:sicl-register-allocation)

(defun extract-locations (arrangement candidates)
  (loop for candidate in candidates
        for attribution = (find candidate (attributions arrangement)
                                :test #'eq :key #'register)
        collect (lexical-location attribution)))

(defun extract-pool-items (pool locations)
  (loop for location in locations
        collect (find location pool :test #'eq :key #'lexical-location)))

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

(defun allocate-register (predecessor instruction pool candidates)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (register (find-unattributed-register attributions candidates)))
    (if (null register)
        ;; There are no unattributed registers of the kind that we are
        ;; looking for.  We must steal one that is already attributed
        ;; to some other lexical location.  We find the lexical
        ;; location with the highest estimated distance to use.
        (flet ((compare (attribution1 attribution2)
                 (let ((pool-item-1 (find (lexical-location attribution1) pool
                                          :key #'lexical-location :test #'eq))
                       (pool-item-2 (find (lexical-location attribution2) pool
                                          :key #'lexical-location :test #'eq)))
                   (> (distance pool-item-1) (distance pool-item-2)))))
          (let* ((attributions
                   (extract-attributions attributions candidates))
                 (sorted (sort attributions #'compare))
                 (attribution (first sorted))
                 (register (register attribution))
                 (stack-slot (stack-slot attribution)))
            ;; We have the attribution we want to steal from.  Now, we
            ;; need to know whether the corresponding lexical location
            ;; is already on the stack as well.
            (if (null stack-slot)
                ;; It is not on the stack. We need to spill the register.
                (values (spill-and-unattribute-register
                         predecessor instruction register)
                        register)
                ;; It is on the stack.  Just unattribute the register.
                (values (unattribute-register predecessor instruction register)
                        register)))))))
