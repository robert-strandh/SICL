(cl:in-package #:sicl-evaluation-and-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MACROEXPAND-1.
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

(defun macroexpand-1 (form &optional env)
  (cond
    ((and (consp form) (symbolp (car form)))
     (let ((expander (macro-function (car form) env)))
       (if (null expander)
	   (values form nil)
	   (values 
	    (funcall (coerce *macroexpand-hook* 'function)
		     expander
		     form
		     (or env sicl-env:*global-environment*))
	    t))))
    ((symbolp form)
     (let* ((environment (or env sicl-env:*global-environment*))
	    (expansion (cleavir-env:symbol-macro-expansion
			form environment)))
       (values expansion
	       (not (eq form expansion)))))
    (t
     (values form nil))))
