(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFGENERIC.

;;; FIXME: Add options and methods
(defmacro defgeneric (&environment env name lambda-list)
  (let* ((arg-type (cleavir-code-utilities:lambda-list-type-specifier lambda-list))
	 (function-type `(function ,arg-type t)))
  `(progn (eval-when (:compile-toplevel)
	    (setf (sicl-global-environment:function-type ',name ,env)
		  ',function-type))
	  (eval-when (:load-toplevel :execute)
	    (ensure-generic-function
	     ',name
	     :lambda-list ',lambda-list
	     :environment
	     (load-time-value
	      (sicl-global-environment:global-environment)))))))
