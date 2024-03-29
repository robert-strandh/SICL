(cl:in-package #:sicl-register-arrangement)

(defclass attribution ()
  ((%lexical-location
    :initarg :lexical-location
    :reader lexical-location)
   (%register-number
    :initform nil
    :initarg :register-number
    :accessor register-number)
   (%stack-slot
    :initform nil
    :initarg :stack-slot
    :accessor stack-slot)))

(defclass arrangement ()
  ((%frozen :initform nil :accessor frozen-p)
   (%stack-map :initarg :stack-map :accessor stack-map)
   (%register-map :initarg :register-map :accessor register-map)
   (%attributions
    :initform '()
    :initarg :attributions
    :accessor attributions)))

(defun check-arrangement-not-frozen (arrangement)
  (when (frozen-p arrangement)
    (error "An update on ~s was attempted but it is frozen." arrangement)))

(defun check-arrangement-integrity (arrangement)
  (let ((register-count 0)
        (register-map (register-map arrangement)))
    ;; Make sure every register in the attributions is accounted for in
    ;; the register map.  Also count the number of registers.
    (loop for attribution in (attributions arrangement)
          for register-number = (register-number attribution)
          do (unless (null register-number)
               (incf register-count)
               (assert (= (bit register-map register-number) 1))))
    ;; Make sure there are as many 1s in the register map as there are
    ;; non-NIL registers in the attributions.
    (assert (= register-count (count 1 register-map)))))

;;; FIXME: remove this method once everything is working.
(defmethod initialize-instance :after ((object arrangement) &key)
  (check-arrangement-integrity object))

(defmacro with-arrangement-parts
    ((stack-map-var register-map-var attributions-var arrangement-form) &body body)
  (let ((arrangement-var (gensym)))
    `(let* ((,arrangement-var ,arrangement-form)
            (,stack-map-var (stack-map ,arrangement-var))
            (,register-map-var (register-map ,arrangement-var))
            (,attributions-var (attributions ,arrangement-var)))
       ,@body)))

(defmacro with-arrangement (arrangement-form &body body)
  `(with-accessors ((stack-map stack-map)
                    (register-map register-map)
                    (attributions attributions))
       ,arrangement-form
     ,@body))

(defmacro with-attribution (attribution-form &body body)
  `(with-accessors ((lexical-location lexical-location)
                    (stack-slot stack-slot)
                    (register-number register-number))
       ,attribution-form
     ,@body))

(defmethod print-object ((arrangement arrangement) stream)
  (print-unreadable-object (arrangement stream :type t :identity t)
    (with-arrangement arrangement
      (format stream "~:a"
              (loop for attribution in attributions
                    collect (cleavir-ir:name
                             (lexical-location attribution)))))))

(defun copy-bit-vector (bit-vector)
  (let ((result (make-array (length bit-vector) :element-type 'bit)))
    (replace result bit-vector)
    result))

(defun copy-attribution (attribution)
  (make-instance 'attribution
    :lexical-location (lexical-location attribution)
    :register-number (register-number attribution)
    :stack-slot (stack-slot attribution)))

(defun copy-attributions (attributions)
  (mapcar #'copy-attribution attributions))

(defun copy-arrangement (arrangement)
  (with-arrangement-parts (stack-map register-map attributions arrangement)
    (make-instance 'arrangement
      :stack-map (copy-bit-vector stack-map)
      :register-map (copy-bit-vector register-map)
      :attributions (copy-attributions attributions))))

;;; Return the attribution of LEXICAL-LOCATION in ARRANGEMENT as two
;;; values, the STACK-SLOT and the REGISTER-NUMBER.  If
;;; LEXICAL-LOCATION is not attributed in ARRANGEMENT, then return NIL
;;; and NIL.
(defun find-attribution (arrangement lexical-location)
  (let ((attribution (find lexical-location (attributions arrangement)
                           :test #'eq :key #'lexical-location)))
    (if (null attribution)
        (values nil nil)
        (values (stack-slot attribution) (register-number attribution)))))

;;; Call FUNCTION with every lexical location, stack slot and register
;;; number.
(defun map-attributions (function arrangement)
  (with-arrangement arrangement
    (dolist (attribution attributions)
      (with-attribution attribution
        (funcall function
                 lexical-location
                 stack-slot
                 register-number)))))

(defun check-arrangement-is-subset (previous next)
  (when (eq previous next)
    (return-from check-arrangement-is-subset))
  (loop for attribution in (attributions next)
        for location = (lexical-location attribution)
        do (multiple-value-bind (previous-stack previous-register)
               (find-attribution previous location)
             (assert (not (and (null previous-register)
                               (null previous-stack)))
                     ()
                     "~S is not attributed in ~S"
                     location previous))))

;;; Determine if the NEXT arrangement is compatible with the PREVIOUS
;;; arrangement, i.e. all the attributions in NEXT are present in the
;;; PREVIOUS arrangement.  Note that compatibility is not symmetric.
(defun arrangements-compatible-p (previous next)
  (check-arrangement-is-subset previous next)
  (or (eq previous next)
      (loop for attribution in (attributions next)
            always (with-attribution attribution
                     (multiple-value-bind (other-register-number other-stack-slot)
                         (find-attribution previous lexical-location)
                       (and (eql other-register-number register-number)
                            (eql other-stack-slot stack-slot)))))))

;;; Destructively update ARRANGEMENT so that the attribution for
;;; LEXICAL-LOCATION has a stack slot.  If there is no attribution for
;;; LEXICAL-LOCATION in ARRANGEMENT, then signal an error.  If the
;;; attribution for LEXICAL-LOCATION already has a stack slot, then
;;; signal an error.
(defun attribute-stack-slot (arrangement lexical-location)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (let ((attribution (find lexical-location attributions
                             :test #'eql :key #'lexical-location)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (assert (null stack-slot))
        (let ((free-stack-slot (find 0 stack-map :test #'eql)))
          (when (null free-stack-slot)
            (setf free-stack-slot (length stack-map))
            (let* ((length (1+ free-stack-slot))
                   (new-stack-map (make-array length :element-type 'bit)))
              (replace new-stack-map stack-map)
              (setf (bit new-stack-map free-stack-slot) 1)
              (setf stack-map new-stack-map)))
          (setf stack-slot free-stack-slot)))))
  (check-arrangement-integrity arrangement))

;;; Destructively update ARRANGEMENT so that LEXICAL-LOCATION has a
;;; register attrbuted to it, selected from CANDIDATES.  If there is
;;; no attribution for LEXICAL-LOCATION, then signal an error.  If the
;;; attribution for LEXICAL-LOCATION already has a register, then
;;; signal an error.  If the attribution for LEXICAL-LOCATION does not
;;; have a stack slot, then signal an error.  If none of the registers
;;; in CANDIDATES is unattributed, then signal an error.
(defun attribute-register-for-existing-lexical-location
    (arrangement lexical-location candidates)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (let ((free-register (position 1 (bit-andc2 candidates register-map)))
          (attribution (find lexical-location attributions
                             :test #'eql :key #'lexical-location)))
      (assert (not (null free-register)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (assert (not (null stack-slot)))
        (assert (null register-number))
        (setf register-number free-register)
        (setf (bit register-map free-register) 1))))
    (check-arrangement-integrity arrangement))

;;; Destructively update ARRANGEMENT so that LEXICAL-LOCATION has a
;;; register attrbuted to it, selected from CANDIDATES.  If there
;;; already an attribution for LEXICAL-LOCATION, then signal an error.
;;; If none of the registers in CANDIDATES is unattributed, then
;;; signal an error.
(defun attribute-register-for-new-lexical-location
    (arrangement lexical-location candidates)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (let ((free-register (position 1 (bit-andc2 candidates register-map)))
          (attribution (find lexical-location attributions
                             :test #'eql :key #'lexical-location)))
      (assert (not (null free-register)))
      (assert (null attribution))
      (push (make-instance 'attribution
              :lexical-location lexical-location
              :stack-slot nil
              :register-number free-register)
            attributions)
      (setf (bit register-map free-register) 1)))
  (check-arrangement-integrity arrangement))

;;; Destructively update ARRANGEMENT so that LEXICAL-LOCATION does not
;;; have a register attributed to it.  If there is no attribution for
;;; LEXICAL-LOCATION, then signal an error.  If the attribution for
;;; LEXICAL-LOCATION does not have a register, then signal an error.
;;; If the attribution for LEXICAL-LOCATION does not have a stack
;;; slot, then signal an error.
(defun unattribute-register (arrangement lexical-location)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (let ((attribution (find lexical-location attributions
                             :test #'eql :key #'lexical-location)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (assert (not (null stack-slot)))
        (assert (not (null register-number)))
        (setf (bit register-map register-number) 0)
        (setf register-number nil))))
  (check-arrangement-integrity arrangement))

(defun reattribute-register
    (arrangement lexical-location candidates)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (let ((free-register (position 1 (bit-andc2 candidates register-map)))
          (attribution (find lexical-location attributions
                             :test #'eql :key #'lexical-location)))
      (assert (not (null free-register)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (setf (bit register-map register-number) 0)
        (setf (bit register-map free-register) 1)
        (setf register-number free-register))))
  (check-arrangement-integrity arrangement))

;;; Return a list of lexical locations such that every lexical
;;; location in the list has some register in CANDIDATES attributed to
;;; it in ARRANGEMENT.
(defun lexical-locations-in-register (arrangement candidates)
  (with-arrangement arrangement
    (loop for attribution in attributions
          nconc (with-attribution attribution
                  (if (or (null register-number)
                          (zerop (bit candidates register-number)))
                      '()
                      (list lexical-location))))))

;;; Return true if and only if LEXICAL-LOCATION has a register
;;; attributed to it in ARRANGEMENT.  If LEXICAL-LOCATION does not
;;; have an attribution in ARRANGEMENT, then an error is signaled.
(defun lexical-location-has-attributed-register-p
    (arrangement lexical-location)
  (with-arrangement arrangement
    (let ((attribution (find lexical-location attributions
                             :test #'eq :key #'lexical-location)))
      (assert (not (null attribution)))
      (not (null (register-number attribution))))))

;;; Return true if and only if LEXICAL-LOCATION has a stack slot
;;; attributed to it in ARRANGEMENT.  If LEXICAL-LOCATION does not
;;; have an attribution in ARRANGEMENT, then an error is signaled.
(defun lexical-location-has-attributed-stack-slot-p
    (arrangement lexical-location)
  (with-arrangement arrangement
    (let ((attribution (find lexical-location attributions
                             :test #'eq :key #'lexical-location)))
      (assert (not (null attribution)))
      (not (null (stack-slot attribution))))))

;;; Return true if and only if LEXICAL-LOCATION has REGISTER-NUMBER
;;; attributed to it in ARRANGEMENT.
(defun lexical-location-in-register-p
    (arrangement lexical-location register-number)
  (with-arrangement arrangement
    (loop for attribution in attributions
          when (eq (lexical-location attribution) lexical-location)
            return (= (register-number attribution)
                      register-number))))

;;; Return the number of unattributed registers in ARRANGEMENT, among
;;; the registers in CANDIDATES.
(defun unattributed-register-count (arrangement candidates)
  (with-arrangement arrangement
    (count 1 (bit-andc2 candidates register-map))))

;;; Return the first stack slot which is unused and has no more used
;;; stack slots past it.  I am not sure if this is a good choice of
;;; function to include in the protocol, but it is necessary for
;;; ADAPT-ARRANGEMENTS-BETWEEN-INSTRUCTIONS and it cannot be
;;; constructed (easily) from other protocol functions.
(defun first-stack-slot-past-arrangement (arrangement)
  (with-arrangement arrangement
    (let ((last-used (position 1 stack-map :from-end t)))
      (if (null last-used)
          (length stack-map)
          (1+ last-used)))))

;;; Return a register map of registers which are unused by all arrangements.
(defun free-registers (arrangement &rest arrangements)
  (bit-not
   (reduce #'bit-ior arrangements
           :key #'register-map
           :initial-value (register-map arrangement))))

;;; Destructively modify arrangement by keeping only the arrangements
;;; with a lexical location that is a member of LEXICAL-LOCATIONS.
(defun trim-arrangement (arrangement lexical-locations)
  (check-arrangement-not-frozen arrangement)
  (check-arrangement-integrity arrangement)
  (with-arrangement arrangement
    (setf attributions
          (loop for attribution in attributions
                if (member (lexical-location attribution) lexical-locations
                           :test #'eq)
                  collect attribution
                else
                  do (with-attribution attribution
                       (unless (null stack-slot)
                         (setf (bit stack-map stack-slot) 0))
                       (unless (null register-number)
                         (setf (bit register-map register-number) 0))))))
  (check-arrangement-integrity arrangement))

;;; Let R be the register attributed to FROM-LEXICAL-LOCATION in
;;; FROM-ARRANGEMENT.  Create an an attribution in TO-ARRANGEMENT that
;;; attributes R to TO-LEXICAL-LOCATION.  No stack slot is attributed
;;; to TO-LEXICAL-LOCATION. in TO-ARRANGEMENT.  If there is no
;;; attribution for FROM-LEXICAL-LOCATION in FROM-ARRANGEMENT, then an
;;; error is signaled.  If no register is attributed to
;;; FROM-LEXICAL-LOCATION in FROM-ARRANGEMENT, then an error is
;;; signaled.  If TO-LEXICAL-LOCATION already has an attribution in
;;; TO-ARRANGEMENT, then an error is signaled.
(defun copy-register-attribution
    (from-arrangement from-lexical-location to-arrangement to-lexical-location)
  (check-arrangement-not-frozen to-arrangement)
  (check-arrangement-integrity from-arrangement)
  (check-arrangement-integrity to-arrangement)
  (let ((from-attribution (find from-lexical-location
                                (attributions from-arrangement)
                                :test #'eq :key #'lexical-location)))
    (assert (not (null from-attribution)))
    (assert (not (null (register-number from-attribution))))
    (assert (null (find to-lexical-location (attributions to-arrangement)
                        :test #'eq :key #'lexical-location)))
    (push (make-instance 'attribution
            :lexical-location to-lexical-location
            :stack-slot nil
            :register-number (register-number from-attribution))
          (attributions to-arrangement))
    (setf (bit (register-map to-arrangement) (register-number from-attribution))
          1))
  (check-arrangement-integrity from-arrangement)
  (check-arrangement-integrity to-arrangement))
