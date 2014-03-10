(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFTYPE.

(defmacro deftype (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (funcall #'(setf type-function)
	      (function ,(sicl-code-utilities:parse-deftype 
			  name
			  lambda-list
			  body))
	      ',name)))

		     
