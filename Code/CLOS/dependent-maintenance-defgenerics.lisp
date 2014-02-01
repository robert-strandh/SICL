(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-dependent.html
(defgeneric add-dependent (metaobject dependent))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-dependent.html
(defgeneric remove-dependent (metaobject dependent))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/map-dependents.html
(defgeneric map-dependents (metaobject function))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/update-dependent.html
(defgeneric update-dependent (metaobject dependent &rest initargs))
