(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; During bootstrapping, we want to be liberal as to what is and what
;;; is not a function.  This can be complicated, so we just give up
;;; for now and say that anything is a function.  This should not be a
;;; problem because this function is called just to detect errors.

(defun functionp (object)
  (declare (ignore object))
  t)
