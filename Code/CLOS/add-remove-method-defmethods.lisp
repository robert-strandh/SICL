(cl:in-package #:sicl-clos)

;;;; This file contains the definition of the specified methods on the
;;;; generic functions ADD-METHOD and REMOVE-METHOD.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function ADD-METHOD.
;;;
;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-method-standard-generic-function-standard-method.html

(defmethod add-method ((generic-function standard-generic-function)
                       (method standard-method))
  (add-method-default generic-function method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method on generic function REMOVE-METHOD.
;;;
;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/remove-method-standard-generic-function-standard-method.html

(defmethod remove-method ((generic-function standard-generic-function)
                          (method standard-method))
  (remove-method-default generic-function method))
