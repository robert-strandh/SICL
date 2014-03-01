(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-method.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_add_me.htm#add-method

(defgeneric add-method (generic-function method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-method.html
;;;
;;; This generic function is also specified in the CLHS.
;;; http://www.lispworks.com/documentation/HyperSpec/Body/f_rm_met.htm#remove-method

(defgeneric remove-method (generic-function method))
