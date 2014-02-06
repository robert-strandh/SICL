(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MISSING.

(defmethod slot-missing
    (class object slot-name operation &optional new-value)
  (slot-missing-default class object operation new-value)
  (error "the slot named ~s is missing from the object ~s"
	 slot-name object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-UNBOUND.

(defmethod slot-unbound (class object slot-name)
  (slot-unbound-default class object slot-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE, (SETF SLOT-VALUE), 
;;; SLOT-VALUE-USING-CLASS (SETF SLOT-VALUE-USING-CLASS)

(defmethod slot-value-using-class ((class standard-class)
				   object
				   (slot standard-effective-slot-definition))
  (slot-value-using-class-default class object slot))

(defmethod slot-value-using-class ((class funcallable-standard-class)
				   object
				   (slot standard-effective-slot-definition))
  (slot-value-using-class-default class object slot))

(defmethod slot-value-using-class ((class built-in-class)
				   object
				   slot)
  (declare (ignore object))
  (error "no slots in an instance of a builtin class"))

(defmethod (setf slot-value-using-class)
  (new-value
   (class standard-class)
   object
   (slot standard-effective-slot-definition))
  (setf (slot-value-using-class-default object slot) new-value))

(defmethod (setf slot-value-using-class)
  (new-value
   (class funcallable-standard-class)
   object
   (slot standard-effective-slot-definition))
  (setf (slot-value-using-class-default object slot) new-value))

(defmethod (setf slot-value-using-class)
  (new-value
   (class built-in-class)
   object
   slot)
  (declare (ignore object))
  (error "no slots in an instance of a builtin class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-BOUNDP SLOT-BOUNDP-USING-CLASS

(defmethod slot-boundp-using-class ((class standard-class)
				    object
				    (slot standard-effective-slot-definition))
  (slot-boundp-using-class-default object slot))

(defmethod slot-boundp-using-class ((class funcallable-standard-class)
				    object
				    (slot standard-effective-slot-definition))
  (slot-boundp-using-class-default object slot))

(defmethod slot-boundp-using-class ((class built-in-class)
				    object
				    slot)
  (declare (ignore object))
  (error "no slots in an instance of a builtin class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-MAKUNBOUND, SLOT-MAKUNBOUND-USING-CLASS.

(defmethod slot-makunbound-using-class
  ((class standard-class)
   object
   (slot standard-effective-slot-definition))
  (slot-makunbound-using-class-default object slot))

(defmethod slot-makunbound-using-class
  ((class funcallable-standard-class)
   object
   (slot standard-effective-slot-definition))
  (slot-makunbound-using-class-default object slot))

(defmethod slot-makunbound-using-class
  ((class built-in-class)
   object
   slot)
  (error "no slots in an instance of a builtin class"))
