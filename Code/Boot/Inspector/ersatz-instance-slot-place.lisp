(cl:in-package #:sicl-boot-inspector)

(defun class-of-object (object)
  (slot-value object 'sicl-boot-phase-3::%class))

(defun rack-of-object (object)
  (slot-value object 'sicl-boot-phase-3::%rack))

(defclass ersatz-instance-slot-place
    (clouseau:key-value-place clouseau:read-only-place)
  ())

(defun rack-element-value (object slot)
  (let ((index (aref (rack-of-object slot) 8)))
    (aref (rack-of-object object) index)))

(defmethod clouseau:value ((place ersatz-instance-slot-place))
  (let ((instance (clouseau:container place))
        (slot (clouseau:cell place)))
    (rack-element-value instance slot)))

(defmethod clouseau:valuep ((place ersatz-instance-slot-place))
  (let ((instance (clouseau:container place))
        (slot (clouseau:cell place)))
    (not (eql (rack-element-value instance slot) 10000000))))
