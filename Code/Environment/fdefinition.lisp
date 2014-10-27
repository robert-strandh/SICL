(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF SYMBOL-FUNCTION).
;;;
;;; According to the HyperSpec, (SETF SYMBOL-FUNCTION) is just like
;;; (SETF FDEFINITION), except that it only accepts a symbol as its
;;; argument.  It suffices thus to check that the argument is a
;;; symbol, and then to call (SETF FDEFINITION) to do the work.

(defun (setf symbol-function) (new-definition symbol)
  (declare (cl:type function new-definition)
	   (cl:type symbol symbol))
  (setf (fdefinition symbol) new-definition))

