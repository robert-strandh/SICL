(cl:in-package #:sicl-compiler-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro MULTIPLE-VALUE-CALL.
;;;
;;; The HyperSpec says that MULTIPLE-VALUE-CALL is a special operator,
;;; but it also says that we have the right to implement any special
;;; operators as a macro.  
;;; 
;;; We are betting that the use of MULTIPLE-VALUE-CALL is going to be
;;; rare, other than in the expansion MULTIPLE-VALUE-BIND (which we
;;; will handle specially), so that performance is not going to be an
;;; issue.
;;; 
;;; For that reason, we expand MULTIPLE-VALUE-CALL to a call to a
;;; function MULTIPLE-VALUE-CALL-FUNCTION.  This function has to do
;;; some unusual things such as saving an unkown number of values of
;;; an unknown number of calls, so it will be implemented separately
;;; for each backend.

(defmacro multiple-value-call (function-form &rest forms)
  `(multiple-value-call-function
    ,function-form
    ,@(mapcar (lambda (form) `(lambda () ,form)) forms)))
