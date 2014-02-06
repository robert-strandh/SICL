(cl:in-package #:sicl-clos)

(defun find-slot (object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class)))
    (find slot-name slots :test #'eq :key #'slot-name)))

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
;;; SLOT-EXISTS-P.

(defun slot-exists-p (object slot-name)
  (not (null (find-slot object slot-name))))

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
    (if (eq value *unbound-value*)
	(slot-unbound class object (slot-definition-name slot))
	value)))

(defun (setf slot-value-using-class-default) (new-value object slot)
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) new-value)
  	(setf (slot-contents (heap-instance-slots object) location)
	      new-value))))

(defun slot-value (object slot-name)
  (let ((class (class-of object)))
    ;; FIXME: check that the object is up to date.  
    ;; 
    ;; The first element of the contents vector is the list of
    ;; effective slots of the class of the object.
    (let* ((slots (slot-contents (heap-instance-slots object) 0))
	   (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
      (if (null slot)
	  (slot-missing class object slot-name 'slot-value)
	  (slot-value-using-class class object slot)))))

(defun (setf slot-value) (new-value object slot-name)
  (let ((class (class-of object)))
    ;; FIXME: check that the object is up to date.  
    ;; 
    ;; The first element of the contents vector is the list of
    ;; effective slots of the class of the object.
    (let* ((slots (slot-contents (heap-instance-slots object) 0))
	   (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
      (if (null slot)
	  (slot-missing class object slot-name 'slot-value)
	  (setf (slot-value-using-class class object slot) new-value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP SLOT-BOUNDP-USING-CLASS

(defun slot-boundp-using-class-default (object slot)
  (let ((location (slot-definition-location slot)))
    (not (eq (if (consp location)
		 (car location)
		 (slot-contents (heap-instance-slots object) location))
	     *unbound-value*))))

(defun slot-boundp (object slot-name)
  ;; FIXME: We must check that the object is a standard instance.
  (let ((slot (find-slot object slot-name))
	(class (class-of object)))
    ;; FIXME: check that the object is up to date.  
    (slot-boundp-using-class class object slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defun slot-makunbound-using-class-default (object slot)
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) *unbound-value*)
  	(setf (slot-contents (heap-instance-slots object) location)
	      *unbound-value*)))
  nil)

(defun slot-makunbound (object slot-name)
  (let* ((slot (find-slot object slot-name))
	 (class (class-of object)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-makunbound)
	(slot-makunbound-using-class class object slot))))
