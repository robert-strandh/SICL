(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CLASS.

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-finalized-p.html
(defgeneric class-finalized-p (class))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/class-prototype.html
(defgeneric class-prototype (class))
