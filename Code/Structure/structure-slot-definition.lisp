(cl:in-package #:sicl-structure)

(defmethod closer-mop:compute-default-initargs ((class structure-class))
  ;; Modify the default initargs behaviour to stop default initargs from
  ;; being inherited from superclasses.
  ;; This, along with help from defstruct, allows the inheritance behaviour
  ;; for slot initforms to be implemented properly.
  (remove-duplicates
   (closer-mop:class-direct-default-initargs class)
   :key #'first :from-end t))

(defclass structure-slot-definition (closer-mop:standard-slot-definition)
  ((%read-only :initarg :read-only :reader structure-slot-definition-read-only)))

(defclass structure-direct-slot-definition (structure-slot-definition
                                            closer-mop:standard-direct-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defclass structure-effective-slot-definition (structure-slot-definition
                                               closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition :around ((class structure-class) name direct-slot-definitions)
  (let ((read-only (structure-slot-definition-read-only (first direct-slot-definitions))))
    ;; Validate the read-only slot. The D-S-D list is sorted in precedence order,
    ;; so the value of read-only slot can only move from true to false, not the
    ;; other way around.
    (let ((current-read-only read-only))
      (dolist (slot direct-slot-definitions)
        (cond (current-read-only
               (setf current-read-only (structure-slot-definition-read-only slot)))
              (t
               ;; Can't go from not read-only to read-only.
               (when (structure-slot-definition-read-only slot)
                 (error 'included-slot-must-be-read-only
                        :slot-name (closer-mop:slot-definition-name slot)))))))
    (let ((effective-slot (call-next-method)))
      (setf (slot-value effective-slot '%read-only) read-only)
      effective-slot)))

(defmethod (setf closer-mop:slot-value-using-class) :before (new-value (class structure-class) object (slot structure-effective-slot-definition))
  (when (and (structure-slot-definition-read-only slot)
             ;; As a special exception, allow unbound/uninitialized slots to
             ;; be initialized.
             (closer-mop:slot-boundp-using-class class object slot))
    (cerror "Set slot anyway" 'slot-is-read-only
            :object object :slot-name (closer-mop:slot-definition-name slot))))

(defmethod closer-mop:slot-makunbound-using-class :before ((class structure-class) object (slot structure-effective-slot-definition))
  (when (and (structure-slot-definition-read-only slot)
             ;; As a special exception, allow unbound/uninitialized slots to
             ;; be initialized.
             (closer-mop:slot-boundp-using-class class object slot))
    (cerror "Make slot unbound anyway" 'slot-is-read-only
            :object object :slot-name (closer-mop:slot-definition-name slot))))
