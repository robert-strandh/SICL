(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/reader-method-class.html
(defgeneric reader-method-class (class direct-slot &rest initargs))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/writer-method-class.html
(defgeneric writer-method-class (class direct-slot &rest initargs))
