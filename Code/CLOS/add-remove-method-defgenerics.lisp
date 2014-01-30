(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-method.html

(defgeneric add-method (generic-function method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-method.html

(defgeneric remove-method (generic-function method))
