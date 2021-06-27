(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-DIRECT-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method.html

(defgeneric add-direct-method (specializer method))

;;; The specification includes two methods on this generic function,
;;; one specialized to CLASS and METHOD and one specialized to
;;; EQL-SPECIALIZER and METHOD.

(defun add-direct-method-default (specializer method)
  (setf (specializer-direct-methods specializer)
        (adjoin method (specializer-direct-methods specializer) :test #'eq)))

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
;;; Generic function REMOVE-DIRECT-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-method.html

(defgeneric remove-direct-method (specializer method))

;;; The specification includes two methods on this generic function,
;;; one specialized to CLASS and METHOD and one specialized to
;;; EQL-SPECIALIZER and METHOD.

(defun remove-direct-method-default (specializer method)
  (setf (specializer-direct-methods specializer)
        (remove method (specializer-direct-methods specializer) :test #'eq)))

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-eql-specializer.html
(defmethod remove-direct-method ((specializer class) (method method))
  (remove-direct-method-default specializer method))

;;; For the specification of this method, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method-eql-specializer.html
(defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (remove-direct-method-default specializer method))
