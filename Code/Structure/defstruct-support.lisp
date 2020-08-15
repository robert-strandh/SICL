(cl:in-package #:sicl-structure)

(defmethod compute-slot-layout (description environment))

(defgeneric layout-slots (description layout))
(defgeneric generate-allocation-form (description layout))
(defgeneric generate-slot-initialization-form (description layout object slot value))

(defgeneric generate-boa-constructor (description layout name lambda-list))
(defgeneric generate-ordinary-constructor (description layout name))

;;; BOA constructors are a more complicated as they have the ability
;;; to completely override any specified slot initforms. So, in some
;;; cases, they need to bypass INITIALIZE-INSTANCE and call
;;; ALLOCATE-INSTANCE/SHARED-INITIALIZE directly.
;;;
;;; If an initform for an &OPTIONAL or &KEY parameter is not provided,
;;; it must default to the slot's initform.
;;; If an initform for an &AUX parameter is not provided, then the
;;; slot associated with that parameter will be left uninitialized,
;;; ignoring any initform specified by the slot.
;;;
;;; 3.4.6 Boa Lambda Lists:
;;; "If no default value is supplied for an aux variable variable, the
;;; consequences are undefined if an attempt is later made to read
;;; the corresponding slot's value before a value is explicitly assigned."
(defmethod generate-boa-constructor (description layout name lambda-list)
  (multiple-value-bind (requireds optionals rest keywords allow-other-keys-p auxs keyp)
      ;; Don't normalize, as BOA lambda lists have different defaulting behaviour
      ;; for initforms.
      (alexandria:parse-ordinary-lambda-list lambda-list :normalize nil)
    (let ((object (gensym "OBJECT"))
          (all-slots (layout-slots description layout))
          (reconstructed-lambda-list '())
          ;; Slots that have been mentioned in the lambda-list and perform
          ;; some kind of initialization.
          (bound-slots '())
          ;; Slots that might be bound to a value or might be left uninitialized
          ;; due to the lack of an initform.
          (potentially-unbound-slots '())
          ;; Slots that have been made unbound by &aux
          (unbound-slots '()))
      (dolist (req requireds)
        (push req reconstructed-lambda-list)
        (push req bound-slots))
      (when optionals
        (push '&optional reconstructed-lambda-list)
        (dolist (opt optionals)
          (let* ((name (if (consp opt)
                           (first opt)
                           opt))
                 (bits (if (consp opt)
                           (rest opt)
                           '()))
                 (slot (find name all-slots :key #'slot-name)))
            (when slot
              (push name bound-slots))
            (cond ((or (not slot)
                       bits)
                   ;; Initform supplied, overrides any initform specified in the defstruct.
                   ;; Or this arg doesn't name a slot at all.
                   (push opt reconstructed-lambda-list))
                  ((slot-initform-p slot)
                   ;; Use the slot initform.
                   (push (list name (slot-initform slot))
                         reconstructed-lambda-list))
                  (t
                   ;; Leave uninitialized.
                   (push (list name ''%uninitialized%)
                         reconstructed-lambda-list)
                   (push name potentially-unbound-slots))))))
      (when rest
        (push '&rest reconstructed-lambda-list)
        (push rest reconstructed-lambda-list)
        (push rest bound-slots))
      (when keyp
        (push '&key reconstructed-lambda-list)
        (dolist (key keywords)
          (let* ((name (cond ((not (consp key)) ; &key foo
                              key)
                             ((consp (first key)) ; &key ((:foo foo) ...)
                              (second (first key)))
                             (t ; &key (foo ...)
                              (first key))))
                 (keyword (cond ((and (consp key) ; &key ((:foo foo) ...)
                                      (consp (first key)))
                                 (first (first key)))
                                (t
                                 (keywordify name))))
                 (bits (if (consp key)
                           (rest key)
                           '()))
                 (slot (find name all-slots :key #'slot-name)))
            (when slot
              (push name bound-slots))
            (cond ((or (not slot)
                       bits)
                   ;; Initform supplied, overrides any initform specified in the defstruct.
                   ;; Or this arg doesn't name a slot at all.
                   (push key reconstructed-lambda-list))
                  ((slot-initform-p slot)
                   ;; Use the slot initform.
                   (push (list (list keyword name) (slot-initform slot))
                         reconstructed-lambda-list))
                  (t
                   ;; Leave uninitialized.
                   (push (list (list keyword name) ''%uninitialized%)
                         reconstructed-lambda-list)
                   (push name potentially-unbound-slots)))))
        (when allow-other-keys-p
          (push '&allow-other-keys reconstructed-lambda-list)))
      (when auxs
        (push '&aux reconstructed-lambda-list)
        (dolist (aux auxs)
          (let* ((name (if (consp aux)
                           (first aux)
                           aux))
                 (bits (if (consp aux)
                           (rest aux)
                           '()))
                 (slot (find name all-slots :key #'slot-name)))
            (cond ((or (not slot)
                       bits)
                   ;; Initform supplied, overrides any initform specified in the defstruct.
                   ;; Or this arg doesn't name a slot at all.
                   (push aux reconstructed-lambda-list)
                   (push name bound-slots))
                  (t
                   ;; Completely override the any initform and leave the
                   ;; slot fully unbound.
                   (push name unbound-slots))))))
      `(defun ,name ,(nreverse reconstructed-lambda-list)
         (let ((,object ,(generate-allocation-form description all-slots)))
           ,@(loop for slot in all-slots
                   for name = (slot-name slot)
                   when (and slot
                             (not (find name unbound-slots))
                             (or (find name bound-slots)
                                 (slot-initform-p slot)))
                     collect (cond ((not (find name bound-slots))
                                    ;; Slot initialized by initform.
                                    (generate-slot-initialization-form description layout object slot (slot-initform slot)))
                                    ((find name potentially-unbound-slots)
                                     ;; Slot may or may not have a value.
                                     `(unless (eq ,name '%uninitialized%)
                                        ,(generate-slot-initialization-form description layout object slot name)))
                                    (t
                                     ;; Slot initialized by argument.
                                     (generate-slot-initialization-form description layout object slot name))))
           ,object)))))

(defmethod generate-ordinary-constructor (description layout constructor-name)
  (let* ((all-slots (layout-slots description layout))
         (suppliedp-syms (loop for slot in all-slots
                               collect (gensym (string (slot-name slot)))))
         ;; "The symbols which name the slots must not be used by the
         ;; implementation as the names for the lambda variables in the
         ;; constructor function, since one or more of those symbols might
         ;; have been proclaimed special or might be defined as the name
         ;; of a constant variable."
         (slot-name-syms (loop for slot in all-slots
                               ;; Use MAKE-SYMBOL instead of GENSYM so the
                               ;; name doesn't look too funny in the lambda-list.
                               collect (make-symbol (string (slot-name slot)))))
         (object (gensym "OBJECT")))
    `(defun ,constructor-name (&key ,@(loop for slot in all-slots
                                            for name in slot-name-syms
                                            for suppliedp in suppliedp-syms
                                            collect (list name (slot-initform slot) suppliedp)))
       (declare (ignorable ,@suppliedp-syms))
       (let ((,object ,(generate-allocation-form description layout)))
         ,@(loop for slot in all-slots
                 for name in slot-name-syms
                 for suppliedp in suppliedp-syms
                 collect (if (slot-initform-p slot)
                             (generate-slot-initialization-form description layout object slot name)
                             ;; If no initform was supplied, then leave the slot uninitialized.
                             `(when ,suppliedp
                                ,(generate-slot-initialization-form description layout object slot name))))
         ,object))))

(defun generate-constructors (description layout)
  (loop for constructor in (defstruct-constructors description)
        collect (if (cdr constructor)
                    (generate-boa-constructor description layout (first constructor) (second constructor))
                    (generate-ordinary-constructor description layout (first constructor)))))

(defgeneric generate-predicate (description layout predicate-name))

(defun generate-predicates (description layout)
  (loop for predicate-name in (defstruct-predicates description)
        collect (generate-predicate description layout predicate-name)))

(defgeneric generate-copier (description layout copier-name))

(defun generate-copiers (description layout)
  (loop for copier-name in (defstruct-copiers description)
        collect (generate-copier description layout copier-name)))

(defgeneric generate-defstruct-bits (description layout))

(defgeneric expand-defstruct (description environment))

(defmethod expand-defstruct (description environment)
  (let ((layout (compute-slot-layout description environment)))
    `(progn
       ,(generate-defstruct-bits description layout)
       ,@(generate-constructors description layout)
       ,@(generate-predicates description layout)
       ,@(generate-copiers description layout)
       ',(defstruct-name description))))

(defun compute-accessor-name (description slot-name)
  (if (defstruct-conc-name description)
      (symbolicate (defstruct-conc-name description)
                   slot-name)
      slot-name))
