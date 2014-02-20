(cl:in-package #:sicl-clos)

(cl:defmethod cl:print-object ((object class) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s u#: ~s"
	    (cl:slot-value object '%name)
	    (cl:slot-value object '%unique-number))))

(cl:defmethod cl:print-object ((object direct-slot-definition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s AL: ~s"
	    (cl:slot-value object '%name)
	    (cl:slot-value object '%allocation))))
  
(cl:defmethod cl:print-object ((object effective-slot-definition) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~s AL: ~s "
	    (cl:slot-value object '%name)
	    (cl:slot-value object '%allocation))
    (when (eq (cl:slot-value object '%allocation) ':instance)
      (if (cl:slot-boundp object '%location)
	  (format stream "L: ~s" (cl:slot-value object '%location))
	  (format stream "no L")))))

  
