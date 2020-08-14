;;;; Expand structure-object-based defstructs.

(cl:in-package #:sicl-structure)

(defun check-included-structure-object (description environment)
  (when (defstruct-included-structure-name description)
    (let* ((parent-name (defstruct-included-structure-name description))
           (included-structure (find-class parent-name environment nil)))
      (unless included-structure
        (if (find-structure-description parent-name nil environment)
            (error "parent defstruct ~S names a typed defstruct, not a structure-object defstruct" parent-name)
            (error "parent defstruct ~S does not exist" parent-name)))
      (unless (typep included-structure 'structure-class)
        (error "parent struct ~S is not a structure-class"
               parent-name))
      ;; All included slots must be present in the included structure.
      (dolist (slot (defstruct-included-slots description))
        (let ((existing (find (slot-name slot)
                              (closer-mop:class-slots included-structure)
                              :key #'closer-mop:slot-definition-name
                              :test #'string=)))
          (unless existing
            (error "included slot ~S does not exist in ~S"
                   (slot-name slot) included-structure))
          ;; For the sake of sanity, lets require them to be the same symbol too.
          ;; If it is legal for them to be different, then they need to be
          ;; canonicalized to the existing slot so that slot inheritance works
          ;; correctly.
          (unless (eql (slot-name slot) (closer-mop:slot-definition-name existing))
            (error "included slot ~S name does not match existing slot name ~S"
                   (slot-name slot) (closer-mop:slot-definition-name existing)))))
      ;; Direct slots must not be present (string=)
      (dolist (slot (defstruct-direct-slots description))
        (when (find (slot-name slot)
                    (closer-mop:class-slots included-structure)
                    :key #'closer-mop:slot-definition-name
                    :test #'string=)
          (error "slot ~S conflicts with slot in included structure ~S"
                 (slot-name slot) included-structure))))))

(defun all-object-slots (description environment)
  ;; All of them, including implicitly included slots.
  (append
   (when (defstruct-included-structure-name description)
     (loop with included-structure = (find-class (defstruct-included-structure-name description)
                                                 environment)
           with included-slots = (defstruct-included-slots description)
           with default-initargs = (closer-mop:class-direct-default-initargs included-structure)
           for slot in (closer-mop:class-slots included-structure)
           for slot-name = (closer-mop:slot-definition-name slot)
           for inclusion = (find slot-name included-slots :key #'slot-name)
           collect (or inclusion
                       ;; For implicitly included slots, reconstruct a slot-description
                       ;; from the effective slot-definition.
                       (let* ((initarg-kw (keywordify slot-name))
                              (default-initarg (find initarg-kw default-initargs :key #'first)))
                         (make-instance 'slot-description
                                        :name slot-name
                                        ;; Generate an accessor with the right name,
                                        ;; don't use the one generated for the parent.
                                        :accessor-name (if (defstruct-conc-name description)
                                                           (symbolicate (defstruct-conc-name description)
                                                                        slot-name)
                                                           slot-name)
                                        :initform (second default-initarg)
                                        :initform-p (not (not default-initarg))
                                        :type (closer-mop:slot-definition-type slot)
                                        :read-only (structure-slot-definition-read-only slot))))))
   (defstruct-direct-slots description)))

(defun compute-structure-object-defclass-slots (all-slots)
  (loop for slot in all-slots
        collect `(,(slot-name slot)
                  :type ,(slot-type slot)
                  :read-only ,(slot-read-only slot)
                  ,(if (slot-read-only slot) :reader :accessor)
                  ,(slot-accessor-name slot))))

(defun compute-structure-object-defclass-default-initargs (all-slots)
  (loop for slot in all-slots
        when (slot-initform-p slot)
        collect (keywordify (slot-name slot))
        and collect (slot-initform slot)))

;;; Ordinary non-BOA constructors are simple, go through the normal
;;; MAKE-INSTANCE path, letting INITIALIZE-INSTANCE take care of slot
;;; initialization.
(defun generate-object-ordinary-constructor (description name all-slots)
  (let* ((slots (mapcar #'slot-name all-slots))
         ;; "The symbols which name the slots must not be used by the
         ;; implementation as the names for the lambda variables in the constructor
         ;; function, since one or more of those symbols might have been proclaimed
         ;; special or might be defined as the name of a constant variable."
         (slot-names (loop for slot in slots
                           ;; Use MAKE-SYMBOL instead of GENSYM so the
                           ;; name doesn't look too funny in the lambda-list.
                           collect (make-symbol (symbol-name slot)))))
    ;; Provide keywords in the lambda-list to improve the development
    ;; experience.
    `(defun ,name (&rest initargs &key ,@slot-names)
       (declare (ignore ,@slot-names))
       (apply #'make-instance ',(defstruct-name description) initargs))))

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
;;;
;;; TODO: Implement the above properly. BOA constructors don't currently
;;; follow the spec but should be good enough for most use cases.
(defun generate-object-boa-constructor (description name lambda-list all-slots)
  (multiple-value-bind (requireds optionals rest keys aok auxs has-keys)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aok has-keys))
    (let ((all-slots (mapcar #'slot-name all-slots))
          ;; Pick out the slot names and compute the slots without a lambda variable
          (assigned-slots (append requireds
                                  (mapcar #'first optionals)
                                  (if rest
                                      (list rest)
                                      '())
                                  (mapcar #'cadar keys) ; (second (first k))
                                  (mapcar #'first auxs)))
          ;; Suppliedp variables aren't used for binding
          (other-vars (append (remove nil (mapcar #'third optionals))
                              (remove 'nil (mapcar #'third keys)))))
      `(defun ,name ,lambda-list
         (declare (ignorable ,@(set-difference (union other-vars assigned-slots)
                                               all-slots)))
         (make-instance
          ',(defstruct-name description)
          ,@(loop for slot in assigned-slots
                  when (member slot all-slots)
                  collect (keywordify slot)
                  and collect slot))))))

(defun generate-object-constructors (description all-slots)
  (loop for constructor in (defstruct-constructors description)
        collect (if (cdr constructor)
                    (generate-object-boa-constructor description (first constructor) (second constructor) all-slots)
                    (generate-object-ordinary-constructor description (first constructor) all-slots))))

(defun expand-object-defstruct (description environment)
  (check-included-structure-object description environment)
  (let ((all-slots (all-object-slots description environment)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,(defstruct-name description)
             (,(or (defstruct-included-structure-name description)
                   'structure-object))
           (,@(compute-structure-object-defclass-slots all-slots))
           (:metaclass structure-class)
           (:default-initargs ,@(compute-structure-object-defclass-default-initargs all-slots))))
       ,@(generate-object-constructors description all-slots)
       ,@(loop for predicate-name in (defstruct-predicates description)
               collect `(defun ,predicate-name (object)
                          (typep object ',(defstruct-name description))))
       ,@(loop for copier-name in (defstruct-copiers description)
               collect `(defun ,copier-name (object)
                          (check-type object ,(defstruct-name description))
                          (copy-structure object)))
       ,@(when (defstruct-print-object description)
           (list `(defmethod print-object ((object ,(defstruct-name description)) stream)
                    (funcall #',(defstruct-print-object description) object stream))))
       ',(defstruct-name description))))
