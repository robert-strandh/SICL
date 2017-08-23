(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-cst
    (cst (info cleavir-env:special-operator-info) env system)
  (convert-special (car (cst:raw cst)) cst env system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

(defmethod convert-cst
    (cst (info cleavir-env:global-macro-info) env system)
  (let ((compiler-macro (cleavir-env:compiler-macro info))
        (form (cst:raw cst)))
    (with-preserved-toplevel-ness
      (if (null compiler-macro)
          ;; There is no compiler macro, so we just apply the macro
          ;; expander, and then convert the resulting form.
          (let* ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
                                         (cleavir-env:expander info)
                                         form
                                         env))
                 (expanded-cst (cst:reconstruct expanded-form cst)))

            (convert expanded-cst env system))
          ;; There is a compiler macro, so we must see whether it will
          ;; accept or decline.
          (let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
                                        compiler-macro
                                        form
                                        env)))
            (if (eq form expanded-form)
                ;; If the two are EQ, this means that the compiler macro
                ;; declined.  Then we appply the macro function, and
                ;; then convert the resulting form, just like we did
                ;; when there was no compiler macro present.
                (let* ((expanded-form (coerce *macroexpand-hook* 'function)
                                      (cleavir-env:expander info)
                                      form
                                      env)
                       (expanded-cst (cst:reconstruct expanded-form cst)))
                  (convert expanded-cst env system))
                ;; If the two are not EQ, this means that the compiler
                ;; macro replaced the original form with a new form.
                ;; This new form must then again be converted without
                ;; taking into account the real macro expander.
                (let ((expanded-cst (cst:reconstruct expanded-form cst)))
                  (convert expanded-cst env system))))))))

(defun make-call (cst info env argument-csts system)
  (let ((function-ast (convert-function-reference info env system))
	(argument-asts (convert-sequence argument-csts env system)))
    (cleavir-ast:make-call-ast function-ast argument-asts)))

(defmethod convert-cst
    (cst (info cleavir-env:global-function-info) env system)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (cleavir-env:eval (cst:raw cst) env env))
  (let ((compiler-macro (cleavir-env:compiler-macro info))
        (notinline (eq 'notinline (cleavir-env:inline info)))
        (form (cst:raw cst)))
    (if (or notinline (null compiler-macro))
        ;; There is no compiler macro.  Create the call.
        (make-call cst info env (cst:rest cst) system)
        ;; There is a compiler macro.  We must see whether it will
        ;; accept or decline.
        (let ((expanded-form (funcall (coerce *macroexpand-hook* 'function)
                                      compiler-macro
                                      form
                                      env)))
          (if (eq form expanded-form)
              ;; If the two are EQ, this means that the compiler macro
              ;; declined.  We are left with function-call form.
              ;; Create the call, just as if there were no compiler
              ;; macro present.
              (make-call cst info env (cdr form) system)
              ;; If the two are not EQ, this means that the compiler
              ;; macro replaced the original form with a new form.
              ;; This new form must then be converted.
              (let ((expanded-cst (cst:reconstruct expanded-form cst)))
                (convert expanded-cst env system)))))))
