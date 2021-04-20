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

;;; Make sure that REGISTER is not attributed to any lexical variable
;;; in the predecessor of INSTRUCTION.
(defun ensure-register-attributions-transferred
    (predecessor instruction pool register)
  (let ((map (make-register-map register))
        (arrangement (output-arrangement predecessor)))
    ;; We do nothing if the register is already unattributed.
    (when (= 1 (arr:unattributed-register-count arrangement map))
      (return-from ensure-register-attributions-transferred predecessor))
    (let ((lexical-locations
            (arr:lexical-locations-in-register arrangement map)))
      (assert (not (null lexical-locations)) ()
              "~S is attributed, but there are no lexical locations attributed to it."
              register)
      (let ((candidates
              (register-map-difference
               (determine-candidates (first lexical-locations) pool)
               map)))
        (cond
          ((plusp
            (arr:unattributed-register-count arrangement candidates))
           ;; There is an unattributed register, so use that register.
           )
          (t
           ;; There are no unattributed registers, so spill a register.
           (let ((result
                   (unattribute-any-register predecessor instruction pool
                                             candidates)))
             (dolist (location lexical-locations)
               (let ((assignment
                       (make-instance 'cleavir-ir:assignment-instruction
                                      :input location
                                      :output location)))
                 (cleavir-ir:insert-instruction-between assignment
                                                        result
                                                        instruction)
                 (setf result assignment)))
             result)))))))

(defun determine-candidates (lexical-location pool)
  (let* ((pool-item (find lexical-location pool
                          :key #'lexical-location :test #'eq)))
    (if (or (null pool-item) (< (call-probability pool-item) 3))
        *caller-saves*
        *callee-saves*)))

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
