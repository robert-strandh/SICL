(in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro expansion.

(defun macro-function (symbol &optional env)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'macro-entry)
			       (eq (name entry) symbol)))
			(append env (macros *global-environment*)))))
    (if (null entry)
	nil
	(definition entry))))

(cl:defparameter *macroexpand-hook*
  (lambda (macro-function macro-form environment)
    (funcall macro-function macro-form environment)))

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
  (let ((expander nil))
    (cond
      ((and (consp form) (symbolp (car form)))
       (setf expander (macro-function (car form) env)))
      ((symbolp form)
       (let ((entry (find-if (lambda (entry)
			       (and (typep entry 'symbol-macro-entry)
				    (eq (name entry) form)))
			     (append env
				     (symbol-macros *global-environment*)))))
	 (if (null entry)
	     nil
	     (setf expander (definition entry)))))
      (t nil))
    (if (null expander)
	(values form nil)
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 env)
		t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MACROEXPAND.
;;;
;;; The HyperSpec says that this function repeatedly calls
;;; MACROEXPAND-1 until the second return value is false, so that is
;;; what we do too.
;;;
;;; However, that is not what the compiler should do.  The reason is
;;; that MACROEXPAND-1 might very well return another macro form that
;;; in addition has a compiler macro associated with it.  In that
;;; case, it would be inappropriate (though technically not "wrong")
;;; for the compiler to call MACROEXPAND-1 again.  Instead, it should
;;; apply the compiler macro first and then expand again.  In fact,
;;; the application of the compiler macro function might return
;;; something other than a macro form. It could also be the case that
;;; what MACROEXPAND-1 returns is NOT a macro form, but it is a
;;; function-call form with a compiler macro associated with it, and
;;; the compiler macro might return a macro form.
;;;
;;; For reasons mentioned in the previous paragraph, the compiler does
;;; not call macroexpand at all, but instead a function called
;;; FULLY-EXPAND-FORM defined below.

(defun macroexpand (form &optional environment)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form environment)
    (if expanded-p
	(loop while (multiple-value-bind (new-expansion expanded-p)
			(macroexpand-1 expansion environment)
		      (setf expansion new-expansion)
		      expanded-p)
	      finally (return (values expansion t)))
	(values form nil))))

