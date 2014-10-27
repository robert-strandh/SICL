(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SYMBOL-FUNCTION.
;;;
;;; According to the HyperSpec, SYMBOL-FUNCTION is just like
;;; FDEFINITION, except that it only accepts a symbol as its argument.
;;; I am guessing that SYMBOL-FUNCTION existed before (SETF <mumble>)
;;; were legal function names, and that FDEFINITION was introduced to
;;; make such names possible.  In fact, on the SYMBOL-FUNCTION page,
;;; the HyperSpec says: (symbol-function symbol) == (fdefinition symbol)
;;; It suffices thus to check that the argument is a symbol, and then 
;;; to call FDEFINITION to do the work. 

(defun symbol-function (symbol)
  (declare (cl:type symbol symbol))
  (fdefinition symbol))

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

