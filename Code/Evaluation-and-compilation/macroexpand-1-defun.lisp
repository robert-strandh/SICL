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

(defun macroexpand-1
    (form &optional (env (sicl-environment:global-environment)))
  (let ((expander nil))
    (cond ((symbolp form)
           (let ((expansion (trucler:symbol-macro-expansion form env)))
             (unless (eq form expansion)
               (setf expander
                     (lambda (form environment)
                       (declare (ignore form environment))
                       expansion)))
             (values expansion (not (eq form expansion)))))
          ((and (consp form) (symbolp (car form)))
           (setf expander (trucler:macro-function (car form) env)))
          (t
           nil))
    (if (null expander)
        (values form nil)
        (values (funcall (coerce *macroexpand-hook* 'function)
                         expander
                         form
                         env)
                t))))
