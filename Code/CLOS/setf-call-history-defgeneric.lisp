(cl:in-package #:sicl-clos)

;;; This function sets the call history of the generic function.
;;; FIXME: say more.
(defgeneric (setf call-history) (new-call-history generic-function))
