(cl:in-package #:sicl-data-and-control-flow)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro MULTIPLE-VALUE-BIND.
;;;
;;; We define this macro pretty much exactly as the HyperSpec says in
;;; the "Notes:" section on the page describing MULTIPLE-VALUE-BIND.  
;;;
;;; Later, we will optimize a bit, either by recognizing this
;;; particular expansion when we compile MULIPLE-VALUE-CALL, or by
;;; handling MULTIPLE-VALUE-BIND as a special operator. 

(defmacro multiple-value-bind (variables values-form &body body)
  (let ((rest-variable (gensym)))
    `(multiple-value-call
	 (lambda (&optional ,@variables &rest ,rest-variable)
	   (declare (ignore ,rest-variable))
	   ,@body)
       ,values-form)))
