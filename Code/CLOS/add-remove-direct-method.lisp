(cl:in-package #:sicl-clos)

;;;; This file contains the definition of the specified methods on the
;;;; generic functions ADD-DIRECT-METHOD and REMOVE-DIRECT-METHOD.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on generic function ADD-METHOD.

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-class.html
(defmethod add-direct-method ((specializer class) (method method))
  (add-direct-method-default specializer method))

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-eql-specializer.html
(defmethod add-direct-method ((specializer eql-specializer) (method method))
  (add-direct-method-default specializer method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods on generic function REMOVE-METHOD.

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-eql-specializer.html
(defmethod remove-direct-method ((specializer class) (method method))
  (remove-direct-method-default specializer method))

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-eql-specializer.html
(defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (remove-direct-method-default specializer method))

