(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-boundp-using-class.html
(defgeneric slot-boundp-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-value-using-class.html
(defgeneric slot-value-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/setf-slot-value-using-class.html
(defgeneric (setf slot-value-using-class) (new-value class object slot))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/slot-makunbound-using-class.html
(defgeneric slot-makunbound-using-class (class object slot))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_mi.htm#slot-missing
(defgeneric slot-missing
    (class object slot-name operation &optional new-value))

;;; For the specification of this generic function, see
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_slt_un.htm#slot-unbound
(defgeneric slot-unbound (class object slot-name))
