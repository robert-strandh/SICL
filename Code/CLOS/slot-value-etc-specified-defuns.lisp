(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-EXISTS-P.

(defun slot-exists-p (object slot-name)
  (not (null (find-slot object slot-name))))

(defun slot-value (object slot-name)
  (let ((slot (find-slot object slot-name))
	(class (class-of object)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-value)
	(slot-value-using-class class object slot))))

(defun (setf slot-value) (new-value object slot-name)
  (let ((slot (find-slot object slot-name))
	(class (class-of object)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-value)
	(setf (slot-value-using-class class object slot) new-value))))

(defun slot-boundp (object slot-name)
  (let ((slot (find-slot object slot-name))
	(class (class-of object)))
    (slot-boundp-using-class class object slot)))

(defun slot-makunbound (object slot-name)
  (let ((slot (find-slot object slot-name))
	(class (class-of object)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-makunbound)
	(slot-makunbound-using-class class object slot))))
