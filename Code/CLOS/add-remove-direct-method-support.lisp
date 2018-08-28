(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function ADD-DIRECT-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/add-direct-method.html
;;;
;;; The specification includes two methods on this generic function,
;;; one specialized for CLASS and METHOD and one specialized for
;;; EQL-SPECIALIZER and METHOD.  The default action below is valid for
;;; both those methods.

(defun add-direct-method-default (specializer method)
  (setf (specializer-direct-methods specializer)
        (adjoin method (specializer-direct-methods specializer) :test #'eq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function REMOVE-DIRECT-METHOD.
;;;
;;; For the specification of this generic function, see
;;; http://metamodular.com/CLOS-MOP/remove-direct-method.html
;;;
;;; The specification includes two methods on this generic function,
;;; one specialized for CLASS and METHOD and one specialized for
;;; EQL-SPECIALIZER and METHOD.  The default action below is valid for
;;; both those methods.

(defun remove-direct-method-default (specializer method)
  (setf (specializer-direct-methods specializer)
        (remove method (specializer-direct-methods specializer) :test #'eq)))
