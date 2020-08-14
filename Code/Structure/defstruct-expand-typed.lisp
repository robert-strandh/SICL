;;;; Expand typed defstructs.

(cl:in-package #:sicl-structure)

(defun check-valid-defstruct-type (description environment)
  ;; The type must name a symbol naming a sequence type,
  ;; or a list that lookes like (VECTOR element-type).
  (let ((type (defstruct-type description)))
    (unless (or (and (symbolp type)
                     (subtypep type 'sequence environment))
                (and (consp type)
                     (eql (first type) 'vector)
                     (consp (rest type))
                     (endp (rest (rest type)))))
      (error "bad defstruct type ~S" type))))

(defun canonicalize-struct-type (type)
  (if (eql type 'vector)
      '(vector t)
      type))

(defun vector-defstruct-p (description)
  (let ((type (defstruct-type description)))
    (or (eql type 'vector)
        (and (consp type)
             (eql (first type) 'vector)))))

(defun compute-typed-defstruct-slot-layout (description environment)
  (let* ((initial-offset (or (defstruct-initial-offset description) 0))
         (initial-offset-padding (make-list initial-offset :initial-element nil)))
    (cond ((defstruct-included-structure-name description)
           (let* ((parent-name (defstruct-included-structure-name description))
                  (parent (find-structure-description parent-name nil environment)))
             (unless parent
               (if (find-class parent-name nil environment)
                   (error "parent defstruct ~S is a structure-object defstruct, not a typed defstruct" parent-name)
                   (error "parent defstruct ~S does not exist" parent-name)))
             ;; TODO: This should do a subtypep test to make sure the types are the same/compatible...
             (unless (equal (canonicalize-struct-type (defstruct-type description))
                            (canonicalize-struct-type (defstruct-type parent)))
               (error "defstruct ~S and ~S have incompatible types ~S and ~S"
                      (defstruct-name description) (defstruct-name parent)
                      (defstruct-type description) (defstruct-type parent)))
             (multiple-value-bind (parent-slot-layout parent-name-layout)
                 (compute-typed-defstruct-slot-layout parent environment)
               ;; Make sure there are no conflicting slots and that all the included
               ;; slots exist.
               (dolist (slot (defstruct-direct-slots description))
                 (when (find (slot-name slot) parent-slot-layout :key #'slot-name :test #'string=)
                   (error "duplicate slot name ~S" (slot-name slot))))
               (dolist (slot (defstruct-included-slots description))
                 (let ((parent-slot (find (slot-name slot) parent-slot-layout :key #'slot-name)))
                   (unless parent-slot
                     (error "included slot ~S missing from parent structure ~S"
                            (slot-name slot) (defstruct-included-structure-name description)))
                   (unless (subtypep (slot-type slot) (slot-type parent-slot) environment)
                     (error "included slot ~S has type ~S, which is not a subtype of ~S"
                            (slot-name slot) (slot-type slot) (slot-type parent-slot)))
                   ;; TODO: Check that the slot type is a subtype of the parent slot.
                   (when (and (slot-read-only parent-slot)
                              (not (slot-read-only slot)))
                     (error "included slot ~S is read-only in parent" (slot-name slot)))))
               ;; Turn the parent-slot-layout into a list of effective-ish slots
               (values (append (loop for parent-slot in parent-slot-layout
                                     collect (or (find (slot-name parent-slot)
                                                       (defstruct-included-slots description)
                                                       :key #'slot-name)
                                                 (make-instance 'slot-description
                                                                :name (slot-name parent-slot)
                                                                :accessor-name (if (defstruct-conc-name description)
                                                                                   (symbolicate (defstruct-conc-name description)
                                                                                                (slot-name parent-slot))
                                                                                   (slot-name parent-slot))
                                                                :initform (slot-initform parent-slot)
                                                                :initform-p (slot-initform-p parent-slot)
                                                                :type (slot-type parent-slot)
                                                                :read-only (slot-read-only parent-slot))))
                               initial-offset-padding
                               (if (defstruct-named description)
                                   (list nil)
                                   '())
                               (defstruct-direct-slots description))
                       (append (if (defstruct-named description)
                                   (list (cons (defstruct-name description) (+ (length parent-slot-layout)
                                                                               initial-offset)))
                                   '())
                               parent-name-layout)))))
          (t
           (if (defstruct-named description)
               (values (append initial-offset-padding
                               (list* nil
                                      (defstruct-direct-slots description)))
                       (list (cons (defstruct-name description) initial-offset)))
               (values (append initial-offset-padding
                               (defstruct-direct-slots description))
                       '()))))))

(defun generate-typed-allocate-instance-call (description slot-layout)
  (if (vector-defstruct-p description)
      `(make-array ,(length slot-layout) :element-type ',(second (canonicalize-struct-type (defstruct-type description))))
      `(make-sequence ',(defstruct-type description) ,(length slot-layout))))

(defun fill-named-fields (object name-layout)
  (loop for (name . name-index) in name-layout
        collect `(setf (elt ,object ,name-index) ',name)))

(defun generate-typed-ordinary-constructor (description constructor-name slot-layout name-layout)
  (let ((suppliedp-syms (loop for slot in slot-layout
                              when slot
                                collect (gensym (string (slot-name slot)))
                              else
                                collect nil))
        ;; "The symbols which name the slots must not be used by the
        ;; implementation as the names for the lambda variables in the
        ;; constructor function, since one or more of those symbols might
        ;; have been proclaimed special or might be defined as the name
        ;; of a constant variable."
        (slot-name-syms (loop for slot in slot-layout
                              when slot
                                ;; Use MAKE-SYMBOL instead of GENSYM so the
                                ;; name doesn't look too funny in the lambda-list.
                                collect (make-symbol (string (slot-name slot)))
                              else
                                collect nil))
        (object (gensym "OBJECT")))
    `(defun ,constructor-name (&key ,@(loop for slot in slot-layout
                                            for name in slot-name-syms
                                            for suppliedp in suppliedp-syms
                                            when slot
                                            collect (list name (slot-initform slot) suppliedp)))
       (declare (ignorable ,@(remove nil suppliedp-syms)))
       (let ((,object ,(generate-typed-allocate-instance-call description slot-layout)))
         ,@(fill-named-fields object name-layout)
         ,@(loop for slot in slot-layout
                 for name in slot-name-syms
                 for suppliedp in suppliedp-syms
                 for index from 0
                 when slot
                 collect (if (slot-initform-p slot)
                             `(setf (elt ,object ,index) ,name)
                             ;; If no initform was supplied, then leave the slot uninitialized.
                             `(when ,suppliedp
                                (setf (elt ,object ,index) ,name))))
         ,object))))

(defun generate-typed-boa-constructor (description name lambda-list slot-layout name-layout)
  (multiple-value-bind (requireds optionals rest keywords allow-other-keys-p auxs keyp)
      ;; Don't normalize, as BOA lambda lists have different defaulting behaviour
      ;; for initforms.
      (alexandria:parse-ordinary-lambda-list lambda-list :normalize nil)
    (let ((object (gensym "OBJECT"))
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
                 (slot (find name slot-layout
                             :key (lambda (x) (and x (slot-name x))))))
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
                   (push (list name '%uninitialized%)
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
                                 (second (first key)))
                                (t
                                 (keywordify name))))
                 (bits (if (consp key)
                           (rest key)
                           '()))
                 (slot (find name slot-layout
                             :key (lambda (x) (and x (slot-name x))))))
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
                   (push (list (list keyword name) '%uninitialized%)
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
                 (slot (find name slot-layout
                             :key (lambda (x) (and x (slot-name x))))))
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
         (let ((,object ,(generate-typed-allocate-instance-call description slot-layout)))
           ,@(fill-named-fields object name-layout)
           ,@(loop for slot in slot-layout
                   for name = (and slot (slot-name slot))
                   for index from 0
                   when (and slot
                             (not (find name unbound-slots))
                             (or (find name bound-slots)
                                 (slot-initform-p slot)))
                     collect (cond ((not (find name bound-slots))
                                    ;; Slot initialized by initform.
                                    `(setf (elt ,object ,index) ,(slot-initform slot)))
                                   ((find name potentially-unbound-slots)
                                    ;; Slot may or may not have a value.
                                    `(unless (eq ,name '%uninitialized%)
                                       (setf (elt ,object ,index) ,name)))
                                   (t
                                    ;; Slot initialized by argument.
                                    `(setf (elt ,object ,index) ,name))))
           ,object)))))

(defun generate-typed-constructors (description slot-layout name-layout)
  (loop for constructor in (defstruct-constructors description)
        collect (if (cdr constructor)
                    (generate-typed-boa-constructor description (first constructor) (second constructor) slot-layout name-layout)
                    (generate-typed-ordinary-constructor description (first constructor) slot-layout name-layout))))

(defun generate-typed-slot-accessor (slot index)
  `((defun ,(slot-accessor-name slot) (structure)
      (the ,(slot-type slot) (elt structure ,index)))
    ,@(unless (slot-read-only slot)
        (list `(defun (setf ,(slot-accessor-name slot)) (new-value structure)
                 (setf (elt structure ,index) (the ,(slot-type slot) new-value)))))))

(defun generate-typed-predicate (description slot-layout name-layout predicate-name)
  `(defun ,predicate-name (object)
     (and (typep object ',(defstruct-type description))
          (>= (length object) ,(length slot-layout))
          ,@(loop for (sym . index) in name-layout
                  collect `(eql (elt object ,index) ',sym)))))

(defun generate-typed-copier (description copier-name)
  (declare (ignore description))
  ;; TODO: Should this check the type?
  `(defun ,copier-name (object)
     (copy-seq object)))

(defun expand-typed-defstruct (description environment)
  (check-valid-defstruct-type description environment)
  (multiple-value-bind (slot-layout name-layout)
      (compute-typed-defstruct-slot-layout description environment)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (find-structure-description ',(defstruct-name description))
               ',description))
       ,@(generate-typed-constructors description slot-layout name-layout)
       ,@(loop for slot in slot-layout
               for index from 0
               when slot
               append (generate-typed-slot-accessor slot index))
       ,@(loop for predicate-name in (defstruct-predicates description)
               collect (generate-typed-predicate description slot-layout name-layout predicate-name))
       ,@(loop for copier-name in (defstruct-copiers description)
               collect (generate-typed-copier description copier-name))
       ',(defstruct-name description))))
