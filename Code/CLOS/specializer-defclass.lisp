(cl:in-package #:sicl-clos)

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-generic-functions.html
(defgeneric specializer-direct-generic-functions (specializer))

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of generic functions
;;; having SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-generic-functions)
    (new-generic-functions specializer))

;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/specializer-direct-methods.html
(defgeneric specializer-direct-methods (specializer))

;;; This function is called by ADD-DIRECT-METHOD and
;;; REMOVE-DIRECT-METHOD so change the list of methods having
;;; SPECIALIZER as a specializer.
(defgeneric (setf specializer-direct-methods) (new-methods specializer))

(defclass specializer (metaobject)
  ((%direct-generic-functions
    :initform '()
    :accessor specializer-direct-generic-functions)
   (%direct-methods
    :initform '()
    :accessor specializer-direct-methods)))
