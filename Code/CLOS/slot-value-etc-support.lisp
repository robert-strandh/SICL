(cl:in-package #:sicl-clos)

(defun find-slot (object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class)))
    (find slot-name slots :test #'eq :key #'slot-definition-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defun slot-missing-default
    (class object slot-name operation &optional new-value)
  (declare (ignore class operation new-value))
  (error "the slot named ~s is missing from the object ~s"
	 slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defun slot-unbound-default (class object slot-name)
  (declare (ignore class))
  (error "the slot named ~s is unbound in the object ~s"
	 slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE), 
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defun slot-value-using-class-default (class object slot)
  (let* ((location (slot-definition-location slot))
	 (value 
	   (if (consp location)
	       (car location)
	       (slot-contents (heap-instance-slots object)
			      location))))
    (if (unbound-value-p value)
	(slot-unbound class object (slot-definition-name slot))
	value)))

(defun (setf slot-value-using-class-default) (new-value class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) new-value)
  	(setf (slot-contents (heap-instance-slots object) location)
	      new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP-USING-CLASS

(defun slot-boundp-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (not (unbound-value-p
	  (if (consp location)
	      (car location)
	      (slot-contents (heap-instance-slots object) location))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defun slot-makunbound-using-class-default (class object slot)
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) *unbound-value*)
  	(setf (slot-contents (heap-instance-slots object) location)
	      (unbound-value))))
  nil)
