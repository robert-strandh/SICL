(cl:in-package #:sicl-clos)

;;; This function sets the specializer profile of the generic function.
;;; FIXME: say more.
(defgeneric (setf specializer-profile) (new-specializer-profile generic-function))
