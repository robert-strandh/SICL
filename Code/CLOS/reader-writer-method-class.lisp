(in-package #:sicl-clos)

(defmethod reader-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  *standard-reader-method*)

(defmethod reader-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  *standard-reader-method*)

(defmethod writer-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  *standard-writer-method*)

(defmethod writer-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  *standard-writer-method*)
