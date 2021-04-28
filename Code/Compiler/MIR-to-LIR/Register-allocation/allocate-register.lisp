(cl:in-package #:sicl-register-allocation)

;;; We know that LEXICAL-LOCATION has an attributed register, and we
;;; need to unattribute that register.  It is possible that
;;; LEXICAL-LOCATION has no stack slot attributed to it.  If that is
;;; the case, we must spill it before unattributing the register
(defun ensure-lexical-location-has-no-attributed-register
    (predecessor instruction lexical-location)
  (let ((arrangement (output-arrangement predecessor)))
    (assert (arr:lexical-location-has-attributed-register-p
             arrangement lexical-location))
    (unattribute-register
     (if (arr:lexical-location-has-attributed-stack-slot-p
          arrangement lexical-location)
         predecessor
         (spill predecessor instruction lexical-location))
     instruction lexical-location)))

(defun augmented-distance (lexical-location pool)
  (let ((pool-item (find lexical-location pool
                         :test #'eq :key #'lexical-location)))
    (if (null pool-item)
        1000000
        (distance pool-item))))

;;; Unattribute any register among the ones in CANDIATES.  We know
;;; that every register in CANDIDATES is attributed to some lexical
;;; location.  So we must choose a "victim" lexical location and
;;; unattribute its register.  We do this by selecting the victim with
;;; the largest estimated distance to use.  Then we call
;;; ENSURE-LEXICAL-LOCATION-HAS-NO-ATTRIBUTED-REGISTER to unattribute
;;; its register.
(defun unattribute-any-register (predecessor instruction pool candidates)
  (let* ((arrangement (output-arrangement predecessor))
         (potential-victims
           (arr:lexical-locations-in-register arrangement candidates)))
    (assert (not (null potential-victims)))
    ;; Designate a lexical location for which we will unattribute its
    ;; attributed register.  We pick the lexical location with the
    ;; greatest estimated distance to use.
    (let* ((victim (first potential-victims))
           (max-distance (augmented-distance victim pool)))
      (loop for potential-victim in (rest potential-victims)
            for distance = (augmented-distance potential-victim pool)
            do (when (> distance max-distance)
                 (setf victim potential-victim)
                 (setf max-distance distance)))
      (ensure-lexical-location-has-no-attributed-register
       predecessor instruction victim))))

;;; Make sure that the output arrangement of the predecessor of
;;; INSTRUCTION is such that there is at least DESIRED-COUNT
;;; unattributed registers among the CANDIDATES.
(defun ensure-unattributed-registers
    (predecessor instruction pool candidates desired-count)
  (let* ((arrangement (output-arrangement predecessor))
         (free-count (arr:unattributed-register-count arrangement candidates))
         (result predecessor))
    (loop repeat (- desired-count free-count)
          do (setf result
                   (unattribute-any-register result instruction pool candidates)))
    result))

;;; Return T when the estimated distance to use of LEXICAL-LOCATION is
;;; higher than that of any location in the list POTENTIAL-VICTIMS.
(defun should-not-transfer-p (lexical-location pool arrangement candidates)
  (loop with location-distance = (augmented-distance lexical-location pool)
        for potential-victim
          in (arr:lexical-locations-in-register arrangement candidates)
        for distance = (augmented-distance potential-victim pool)
        when (> distance location-distance)
          return t)
  nil)

(defun filter-for-lexical-location (lexical-location)
  *gpr*)

(defun determine-candidates (lexical-location pool
                             &key (filter
                                   (filter-for-lexical-location lexical-location)))
  (let* ((pool-item (find lexical-location pool
                          :key #'lexical-location :test #'eq)))
    (if (or (null pool-item) (< (call-probability pool-item) 3))
        (register-map-intersection *caller-saves* filter)
        (let ((register-map
                (register-map-intersection *callee-saves* filter)))
          (if (register-map-empty-p register-map)
              (register-map-intersection *caller-saves* filter)
              register-map)))))

(defun lexical-location-in-register-p (arrangement lexical-location register)
  (arr:lexical-location-in-register-p
   arrangement
   lexical-location
   (register-number register)))

;;; Make sure that REGISTER is not attributed to any lexical variable
;;; in the predecessor of INSTRUCTION.
(defun ensure-register-attributions-transferred
    (predecessor instruction pool register)
  (let* ((map (make-register-map register))
         (arrangement (output-arrangement predecessor))
         (lexical-locations
           (arr:lexical-locations-in-register arrangement map)))
    ;; We do nothing if the register is already unattributed.
    (when (null lexical-locations)
      (return-from ensure-register-attributions-transferred predecessor))
    (let* ((location (first lexical-locations))
           (candidates
             (register-map-difference (determine-candidates location pool) map)))
      ;; We do nothing if the location will not live past this instruction.
      (unless (variable-live-p location (output-pool instruction))
        (return-from ensure-register-attributions-transferred predecessor))
      ;; If this location has a higher EDU than any other which is
      ;; attributed, spill this location.
      (when (should-not-transfer-p location pool arrangement candidates)
        (return-from ensure-register-attributions-transferred
          (spill predecessor instruction location)))
      ;; Else, spill some other register and transfer the location to
      ;; that register.
      (when (zerop (arr:unattributed-register-count arrangement candidates))
        ;; There are no unattributed registers, so spill one.
        (setf predecessor
              (ensure-unattributed-registers predecessor
                                             instruction
                                             pool
                                             candidates
                                             1)
              arrangement (output-arrangement predecessor)))
      ;; There is now an unattributed register, so use that register.
      (let ((new-arrangement (arr:copy-arrangement arrangement))
            (assignment
              (make-instance 'cleavir-ir:assignment-instruction
                             :input location
                             :output location)))
        (arr:unattribute-register new-arrangement location)
        (arr:attribute-register-for-new-lexical-location
         new-arrangement
         location
         candidates)
        (setf (output-arrangement assignment) new-arrangement
              (input-arrangement  assignment) arrangement)
        (cleavir-ir:insert-instruction-between
         assignment
         predecessor
         instruction)
        assignment))))

;;; Ensure that LEXICAL-LOCATION has an attributed register.  We
;;; account for two possibilities.  If LEXICAL-LOCATION already has an
;;; attributed register, then we return PREDECESSOR.  If not, we first
;;; determine a set of candidate registers for LEXICAL-LOCATION, based
;;; on the contents of the input pool of INSTRUCTION.  Then we call
;;; ENSURE-UNATTRIBUTED-REGISTER to make sure one of the candidate
;;; registers is unattributed.  Finally, we call UNSPILL to make sure
;;; LEXICAL-LOCATION has one of the candidate registers allocated to
;;; it.
(defun ensure-lexical-location-has-attributed-register
    (predecessor instruction lexical-location)
  (let ((arrangement (output-arrangement predecessor)))
    (if (arr:lexical-location-has-attributed-register-p
         arrangement lexical-location)
        predecessor
        (let* ((pool (input-pool instruction))
               (candidates (determine-candidates lexical-location pool))
               (new-predecessor
                 (ensure-unattributed-registers
                  predecessor instruction pool candidates 1)))
          (unspill new-predecessor
                   instruction
                   lexical-location
                   candidates)))))

;;; Ensure that LEXICAL-LOCATION has an attributed stack slot.  We
;;; account for two possibilities.  If LEXICAL-LOCATION already has an
;;; attributed stack slot, then we return PREDECESSOR.  If not, we
;;; call SPILL to accomplish the task.
(defun ensure-lexical-location-has-attributed-stack-slot
    (predecessor instruction lexical-location)
  (let ((arrangement (output-arrangement predecessor)))
    (if (arr:lexical-location-has-attributed-stack-slot-p
         arrangement lexical-location)
        predecessor
        (spill predecessor instruction lexical-location))))
