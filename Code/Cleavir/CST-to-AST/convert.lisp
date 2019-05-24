(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a concrete syntax tree to an
;;; abstract syntax tree.

(defmethod convert (cst environment system)
  (let ((form (cst:raw cst)))
    (cond ((and (not (consp form)) (not (symbolp form)))
           (convert-constant cst environment system))
          ((symbolp form)
           (convert-variable cst environment system))
          ((symbolp (car form))
           ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
           ;; do not know whether to evaluate the form at compile time,
           ;; simply because it might be a special form that is handled
           ;; specially.  So we must wait until we have more
           ;; information.
           (let ((info (function-info environment (car form))))
             (convert-cst cst info environment system)))
          (t
           ;; The form must be a compound form where the CAR is a lambda
           ;; expression.  Evaluating such a form might have some
           ;; compile-time side effects, so we must check whether we are
           ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
           ;; the form as well.
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (cleavir-env:eval form environment environment))
           (convert-lambda-call cst environment system)))))

(defmethod convert :around (cst environment system)
  (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
        (*subforms-are-top-level-p* nil)
        ;; gives all generated ASTs the appropriate policy.
        (cleavir-ast:*policy*
          (cleavir-env:environment-policy environment)))
    (restart-case
        (call-next-method)
      (continue ()
        :report "Replace with call to ERROR."
        (convert (cst:cst-from-expression
                  `(error 'run-time-program-error
                          :expr ',(cst:raw cst)
                          :origin ',(cst:source cst)))
                 environment system)))))
