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
           (max-distance (distance-of-lexical-location victim pool)))
      (loop for potential-victim in (rest potential-victims)
            for distance = (distance-of-lexical-location potential-victim pool)
            do (when (> distance max-distance)
                 (setf victim potential-victim)
                 (setf max-distance distance)))
      (ensure-lexical-location-has-no-attributed-register
       predecessor instruction victim))))

;;; Make sure that the output arrangement of the predecessor of
;;; INSTRUCTION is such that there is at least one unattributed
;;; register among the CANDIDATES.
(defun ensure-unattributed-register (predecessor instruction pool candidates)
  (let ((arrangement (output-arrangement predecessor)))
    (if (plusp (arr:unattributed-register-count arrangement candidates))
        predecessor
        (unattribute-any-register predecessor instruction pool candidates))))

(defun determine-candidates (lexical-location pool)
  (let* ((pool-item (find lexical-location pool
                          :key #'lexical-location :test #'eq))
         (call-probability (call-probability pool-item)))
    (if (>= call-probability 3) *callee-saves* *caller-saves*)))

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
                 (ensure-unattributed-register
                  predecessor instruction pool candidates)))
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
