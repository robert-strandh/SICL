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

(defun compute-included-structure-object-slots (description environment)
  ;; All effective slots, not just explicitly included slots, need to be
  ;; included so that the correct accessor methods are generated.
  (when (defstruct-included-structure-name description)
    (loop with included-structure = (find-class (defstruct-included-structure-name description)
                                                environment)
          for slot in (closer-mop:class-slots included-structure)
          for inclusion = (find (closer-mop:slot-definition-name slot)
                                (defstruct-included-slots description)
                                :key #'slot-name)
          collect (if inclusion
                      ;; Explicitly included slot.
                      `(,(closer-mop:slot-definition-name slot)
                        :type ,(slot-type inclusion)
                        :read-only ,(slot-read-only inclusion)
                        ,(if (slot-read-only inclusion) :reader :accessor)
                        ,(slot-accessor-name inclusion))
                      ;; Implicitly included slot, only include the accessor option.
                      (list (closer-mop:slot-definition-name slot)
                            (if (structure-slot-definition-read-only slot) :reader :accessor)
                            ;; An ugly wart, we have to generate the name here.
                            (if (defstruct-conc-name description)
                                (symbolicate (defstruct-conc-name description)
                                             (closer-mop:slot-definition-name slot))
                                (closer-mop:slot-definition-name slot)))))))

(defun compute-direct-structure-object-slots (description)
  (loop for slot in (defstruct-direct-slots description)
        collect `(,(slot-name slot)
                  :initarg ,(keywordify (slot-name slot))
                  :type ,(slot-type slot)
                  :read-only ,(slot-read-only slot)
                  ,(if (slot-read-only slot) :reader :accessor)
                  ,(slot-accessor-name slot))))

(defun compute-structure-object-direct-default-initargs (description environment)
  ;; All effective slots, not just explicitly included slots, need to be
  ;; included so that initforms are evaluated in the correct lexical environment.
  (when (defstruct-included-structure-name description)
    (loop with included-structure = (find-class (defstruct-included-structure-name description)
                                                environment)
          for slot in (closer-mop:class-slots included-structure)
          for initarg-kw = (keywordify (closer-mop:slot-definition-name slot))
          for inclusion = (find (closer-mop:slot-definition-name slot)
                                (defstruct-included-slots description)
                                :key #'slot-name)
          append (cond (inclusion
                        ;; Explicitly included slot.
                        ;; Suppress initform if requested.
                        (if (slot-initform-p inclusion)
                            (list initarg-kw (slot-initform inclusion))
                            '()))
                       (t
                        (let* ((default-initargs (closer-mop:class-direct-default-initargs included-structure))
                               (default-initarg (find initarg-kw default-initargs :key #'first)))
                          (if default-initarg
                              (list initarg-kw (second default-initarg))
                              '())))))))

(defun compute-structure-object-direct-default-initargs (description)
  (loop for slot in (defstruct-direct-slots description)
        when (slot-initform-p slot)
        collect (keywordify (slot-name slot))
        and collect (slot-initform slot)))

(defun all-object-slot-names (description environment)
  ;; All of them, including implicitly included slots.
  (append
   (when (defstruct-included-structure-name description)
     (loop with included-structure = (find-class (defstruct-included-structure-name description)
                                                 environment)
           for slot in (closer-mop:class-slots included-structure)
           collect (closer-mop:slot-definition-name slot)))
   (loop for slot in (defstruct-direct-slots description)
         collect (slot-name slot))))

;;; Ordinary non-BOA constructors are simple, go through the normal
;;; MAKE-INSTANCE path, letting INITIALIZE-INSTANCE take care of slot
;;; initialization.
(defun generate-object-ordinary-constructor (description environment name)
  (let* ((slots (all-object-slot-names description environment))
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
(defun generate-object-boa-constructor (description environment name lambda-list)
  (multiple-value-bind (requireds optionals rest keys aok auxs has-keys)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (declare (ignore aok has-keys))
    (let ((all-slots (all-object-slot-names description environment))
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

(defun expand-object-defstruct (description environment)
  (check-included-structure-object description environment)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,(defstruct-name description)
           (,(or (defstruct-included-structure-name description)
                 'structure-object))
         (,@(compute-included-structure-object-slots description environment)
          ,@(compute-direct-structure-object-slots description))
         (:metaclass structure-class)
         (:default-initargs ,@(compute-structure-object-included-default-initargs description environment)
                            ,@(compute-structure-object-direct-default-initargs description))))
     ,@(loop for constructor in (defstruct-constructors description)
             collect (if (cdr constructor)
                         (generate-object-boa-constructor description environment (first constructor) (second constructor))
                         (generate-object-ordinary-constructor description environment (first constructor))))
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
     ',(defstruct-name description)))
