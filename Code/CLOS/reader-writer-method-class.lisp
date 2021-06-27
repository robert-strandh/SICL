(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/reader-method-class.html
(defgeneric reader-method-class (class direct-slot &rest initargs))

(defmethod reader-method-class
    ((class regular-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod reader-method-class
    ((class built-in-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/writer-method-class.html
(defgeneric writer-method-class (class direct-slot &rest initargs))

(defmethod writer-method-class
    ((class regular-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))

(defmethod writer-method-class
    ((class built-in-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))
