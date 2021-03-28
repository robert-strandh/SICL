(cl:in-package #:sicl-register-allocation)

(defun extract-locations (arrangement candidates)
  (loop for candidate in candidates
        for attribution = (find candidate (attributions arrangement)
                                :test #'eql :key #'register-number)
        collect (lexical-location attribution)))

(defun extract-pool-items (pool locations)
  (loop for location in locations
        collect (find location pool :test #'eq :key #'lexical-location)))

;;; Return the register that is attributed to LEXICAL-LOCATION in
;;; ARRANGEMENT, or NIL if no register is atributed to
;;; LEXICAL-LOCATION.  We know that LEXICAL-LOCATION is live, so we
;;; know that it has an attribution in ASSIGNEMENT.  We just don't
;;; know whether the REGISTER-NUMBER of that attribution is a register
;;; or NIL.
(defun attributed-register (lexical-location arrangement)
  (register-number (find lexical-location (attributions arrangement)
                         :key #'lexical-location :test #'eq)))

;;; From ATTRIBUTIONS, extract attributions with a register that is in
;;; REGISTER-MAP.
(defun extract-attributions (attributions register-map)
  (loop for attribution in attributions
        when (register-number-in-map-p
              (register-number attribution) register-map)
          collect attribution))

;;; If there is at least one register in CANDIDATE-REGISTER-MAP that is not in
;;; any attribution in ARRANGEMENT, then return any such register.
;;; Otherwise return NIL.
(defun find-unattributed-register (arrangement candidate-register-map)
  (let ((attributed-registers (register-map arrangement)))
    (let ((unattributed-register-map
            (register-map-difference candidate-register-map attributed-registers)))
      (find-any-register-in-map unattributed-register-map))))

;;; By "allocating a register", we mean to make sure that the output
;;; arrangment of the predecessor of INSTRUCTION does not have the
;;; register in any of its attributions.  To accomplish this task, we
;;; may have to add new instructions preceding INSTRUCTION, and it is
;;; the last one of these added instructions that will correspond to
;;; the contract.  For that reason, we return two values: A new
;;; predecessor and the register we allocated.
(defun allocate-register (predecessor instruction pool candidates)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (register-number (find-unattributed-register arrangement candidates)))
    (if (null register-number)
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
                 (register-number (register-number attribution))
                 (stack-slot (stack-slot attribution)))
            ;; We have the attribution we want to steal from.  Now, we
            ;; need to know whether the corresponding lexical location
            ;; is already on the stack as well.
            (if (null stack-slot)
                ;; It is not on the stack. We need to spill the register.
                (values (spill-and-unattribute-register
                         predecessor instruction register-number)
                        register-number)
                ;; It is on the stack.  Just unattribute the register.
                (values (unattribute-register
                         predecessor instruction register-number)
                        register-number))))
        ;; There is an unattributed register of the kind that we are
        ;; looking for.  This means that PREDECESSOR already fulfuls
        ;; the contract, so we can use it as it is.
        (values predecessor register-number))))

(defun determine-candidates (lexical-location pool)
  (let* ((pool-item (find lexical-location pool
                          :key #'lexical-location :test #'eq))
         (call-probability (call-probability pool-item)))
    (if (>= call-probability 3) *callee-saves* *caller-saves*)))

(defun ensure-in-register (lexical-location predecessor instruction)
  (let* ((arrangement (output-arrangement predecessor))
         (attributions (attributions arrangement))
         (attribution (find lexical-location attributions
                            :key #'lexical-location :test #'eq))
         (register-number (register-number attribution))
         (pool (input-pool instruction))
         (pool-item (find lexical-location pool
                          :key #'lexical-location :test #'eq))
         (call-probability (call-probability pool-item)))
    (if (null register-number)
        (allocate-register
         predecessor
         instruction
         pool
         (if (>= call-probability 3) *callee-saves* *caller-saves*))
        (values predecessor register-number))))
