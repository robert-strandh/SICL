(cl:in-package #:sicl-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MACROEXPAND-1.
;;;
;;; This function is a generic version of CL:MACROEXPAND-1.  It takes
;;; a required environment parameter so that methods can be written
;;; that specialize on the environment parameter.
;;; 
;;; This function returns two values, the expanded form and a boolean
;;; indicating whether the form was expanded or not.  I can't think of
;;; a reason for the existence of the second return value, because if
;;; the form is a macro form, then the expander is called and the
;;; second value is true.  If the form is not a macro form, then the
;;; initial form is return and the second value is false.  If the
;;; expander should ever return the same form, then MACROEXPAND-1 will
;;; be called again (at least if it was called by MACROEXPAND) and we
;;; would have an infinite computation.  Though I suppose that if the
;;; expander did some non-functional stuff like consulting global
;;; variables, then, it might be possible. 

(defgeneric macroexpand-1 (form environment))

(defmethod macroexpand-1 (form env)
  (let ((expander nil))
    (cond ((symbolp form)
	   (let ((expansion (cleavir-env:symbol-macro-expansion form env)))
	     (unless (eq form expansion)
	       (setf expander
		     (lambda (form environment)
		       (declare (ignore form environment))
		       expansion)))
	     (values expansion (not (eq form expansion)))))
	  ((and (consp form) (symbolp (car form)))
	   (setf expander (cleavir-env:macro-function (car form) env)))
	  (t
	   nil))
    (if (null expander)
	(values form nil)
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 env)
		t))))
