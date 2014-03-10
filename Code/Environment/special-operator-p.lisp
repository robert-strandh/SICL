(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SPECIAL-OPERATOR-P.
;;;
;;; We could return something more useful than T, but since conforming
;;; code can not count on anything else, we might as well just return
;;; T.

(defun special-operator-p (symbol)
  (declare (cl:type symbol symbol))
  (let ((entry (find symbol (special-operators *global-environment*)
		     :key #'name :test #'eq)))
    (not (null entry))))

