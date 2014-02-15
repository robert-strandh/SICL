(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; During bootstrapping, we want to be liberal as to what is and what
;;; is not a direct slot definition.  This can be complicated, so we
;;; just give up for now and say that anything is a direct slot
;;; definition.  This should not be a problem because this function is
;;; called just to detect errors.

(defun direct-slot-definition-p (object)
  (declare (ignore object))
  t)
