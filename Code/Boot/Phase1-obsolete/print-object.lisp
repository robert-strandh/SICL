(cl:in-package #:sicl-boot-phase1)

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s u#: ~s"
	    (class-name object)
	    (unique-number object))))

(defmethod print-object ((object direct-slot-definition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s AL: ~s"
	    (slot-definition-name object)
	    (slot-definition-allocation object))))

(defmethod print-object ((object effective-slot-definition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s AL: ~s "
	    (slot-definition-name object)
	    (slot-definition-allocation object))
    (when (eq (slot-definition-allocation object) ':instance)
      (if (slot-boundp object '%location)
	  (format stream "L: ~s" (slot-definition-location object))
	  (format stream "no L")))))

  
