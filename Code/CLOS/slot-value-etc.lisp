(in-package #:sicl-clos-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIXME: make all this faster by going through the instance slots
;;; rather than the class slots.  This requires that the instance be
;;; up to date. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-EXISTS-P.

(defun slot-exists-p (object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class))
	 (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
    (not (null slot))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defgeneric slot-missing
    (class object slot-name operation &optional new-value))

(defmethod slot-missing
    (class object slot-name operation &optional new-value)
  (error "the slot named ~s is missing from the object ~s"
	 slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defgeneric slot-unbound (class object slot-name))

(defmethod slot-unbound (class object slot-name)
  (error "the slot named ~s is unbound in the object ~s"
	 slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE), 
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defgeneric slot-value-using-class (class object slot))

(defmethod slot-value-using-class ((class standard-class)
				   object
				   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let* ((location (slot-definition-location slot))
	 (value 
	   (if (consp location)
	       (car location)
	       (slot-contents (standard-instance-slots object)
			      location))))
    (if (eq value *secret-unbound-value*)
	(slot-unbound class object (slot-definition-name slot))
	value)))

(defmethod slot-value-using-class ((class funcallable-standard-class)
				   object
				   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let* ((location (slot-definition-location slot))
	 (value 
	   (if (consp location)
	       (car location)
	       (slot-contents (standard-instance-slots object)
			      location))))
    (if (eq value *secret-unbound-value*)
	(slot-unbound class object (slot-definition-name slot))
	value)))

(defmethod slot-value-using-class ((class built-in-class)
				   object
				   slot)
  (error "no slots in an instance of a builtin class"))

(defgeneric (setf slot-value-using-class) (new-value class object slot))

(defmethod (setf slot-value-using-class)
  (new-value
   (class standard-class)
   object
   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) new-value)
  	(setf (slot-contents (standard-instance-slots object) location)
	      new-value))))

(defmethod (setf slot-value-using-class)
  (new-value
   (class funcallable-standard-class)
   object
   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) new-value)
  	(setf (slot-contents (standard-instance-slots object) location)
	      new-value))))

(defmethod (setf slot-value-using-class)
  (new-value
   (class built-in-class)
   object
   slot)
  (error "no slots in an instance of a builtin class"))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class))
	 (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-value)
	(slot-value-using-class class object slot))))

(defun (setf slot-value) (new-value object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class))
	 (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-value)
	(setf (slot-value-using-class class object slot) new-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP SLOT-BOUNDP-USING-CLASS

(defgeneric slot-boundp-using-class (class object slot))

(defmethod slot-boundp-using-class ((class standard-class)
				    object
				    (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (not (eq (if (consp location)
		 (car location)
		 (slot-contents (standard-instance-slots object) location))
	     *secret-unbound-value*))))

(defmethod slot-boundp-using-class ((class funcallable-standard-class)
				    object
				    (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (not (eq (if (consp location)
		 (car location)
		 (slot-contents (standard-instance-slots object) location))
	     *secret-unbound-value*))))

(defmethod slot-boundp-using-class ((class built-in-class)
				    object
				    slot)
  (error "no slots in an instance of a builtin class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defgeneric slot-makunbound-using-class (class object slot))

(defmethod slot-makunbound-using-class
  ((class standard-class)
   object
   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) *secret-unbound-value*)
  	(setf (slot-contents (standard-instance-slots object) location)
	      *secret-unbound-value*))))

(defmethod slot-makunbound-using-class
  ((class funcallable-standard-class)
   object
   (slot standard-effectove-slot-definition))
  (declare (ignore class))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
	(setf (car location) *secret-unbound-value*)
  	(setf (slot-contents (standard-instance-slots object) location)
	      *secret-unbound-value*))))

(defmethod slot-makunbound-using-class
  ((class built-in-class)
   object
   slot)
  (error "no slots in an instance of a builtin class"))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
	 (slots (class-slots class))
	 (slot (find slot-name slots :test #'eq :key #'slot-definition-name)))
    (if (null slot)
	(slot-missing class object slot-name 'slot-makunbound)
	(slot-makunbound-using-class class object slot))))

    