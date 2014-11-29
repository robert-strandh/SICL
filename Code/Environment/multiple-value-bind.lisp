(cl:in-package #:sicl-global-environment)

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
  (unless (cleavir-code-utilities:proper-list-p variables)
    (error 'variables-must-be-proper-list :variables variables))
  (let ((non-symbol (find-if-not #'symbolp variables)))
    (unless (null non-symbol)
      (error 'variable-must-be-symbol :variable non-symbol)))
  (let ((rest-variable (gensym)))
    `(multiple-value-call
	 (lambda (&optional ,@variables &rest ,rest-variable)
	   (declare (ignore ,rest-variable))
	   ,@body)
       ,values-form)))
