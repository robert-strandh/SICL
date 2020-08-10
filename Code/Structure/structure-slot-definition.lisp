(cl:in-package #:sicl-structure)

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
                 (error "slot ~S in class ~S has bad read-onlyness"
                        slot class))))))
    (let ((effective-slot (call-next-method)))
      (setf (slot-value effective-slot '%read-only) read-only)
      effective-slot)))
