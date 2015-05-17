(cl:in-package #:sicl-boot-phase2)

(defgeneric print-object (object stream))

(cl:defmethod cl:print-object ((object sicl-boot-phase1::heap-instance) stream)
  (print-object object stream))

(defmethod print-object (object stream)
  (print-unreadable-object (object stream)
    (format stream "*some ersatz-object")))

(defmethod print-object ((object function) stream)
  (print-unreadable-object (object stream)
    (format stream "*a function")))

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream)
    (format stream "*a standard-object")))

(defmethod print-object ((object funcallable-standard-object) stream)
  (print-unreadable-object (object stream)
    (format stream "*a funcallable-standard-object")))

(defmethod print-object ((object metaobject) stream)
  (print-unreadable-object (object stream)
    (format stream "*a metaobject")))

(defmethod print-object ((object generic-function) stream)
  (print-unreadable-object (object stream)
    (format stream "*a generic function")))

(defmethod print-object ((object standard-generic-function) stream)
  (print-unreadable-object (object stream)
    (format stream "*~s ~s"
	    (class-name (heap-instance-class object))
	    (generic-function-name object))))

(defmethod print-object ((object method) stream)
  (print-unreadable-object (object stream)
    (format stream "*a method")))

(defmethod print-object ((object standard-method) stream)
  (print-unreadable-object (object stream)
    (format stream "*a standard method")))

(defmethod print-object ((object standard-accessor-method) stream)
  (print-unreadable-object (object stream)
    (format stream "*a standard accessor method")))

(defmethod print-object ((object standard-reader-method) stream)
  (print-unreadable-object (object stream)
    (format stream "*a standard reader method")))

(defmethod print-object ((object standard-writer-method) stream)
  (print-unreadable-object (object stream)
    (format stream "*a standard writer method")))

(defmethod print-object ((object slot-definition) stream)
  (format stream "*~s ~s"
	  (class-name (heap-instance-class object))
	  (slot-definition-name object)))

(defmethod print-object ((object method-combination) stream)
  (print-unreadable-object (object stream)
    (format stream "*a method combination")))

(defmethod print-object ((object specializer) stream)
  (print-unreadable-object (object stream)
    (format stream "*a specializer")))

(defmethod print-object ((object eql-specializer) stream)
  (print-unreadable-object (object stream)
    (format stream "*an eql specializer")))

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream)
    (format stream "*~s ~s"
	    (class-name (heap-instance-class object))
	    (class-name object))))
