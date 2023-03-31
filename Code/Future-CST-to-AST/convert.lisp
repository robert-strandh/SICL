(cl:in-package #:sicl-expression-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a concrete syntax tree to an
;;; abstract syntax tree.

(defmethod convert (client cooked-expression environment)
  (let ((form (c:raw cooked-expression)))
    (cond ((or (and (not (consp form)) (not (symbolp form)))
               (keywordp form)
               (member form '(t nil)))
           (convert-constant client cooked-expression environment))
          ((symbolp form)
           (convert-variable client cooked-expression environment))
          ((symbolp (car form))
           ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
           ;; do not know whether to evaluate the form at compile time,
           ;; simply because it might be a special form that is handled
           ;; specially.  So we must wait until we have more
           ;; information.
           (let ((info (describe-function client environment (c:first cooked-expression))))
             (convert-with-description client cooked-expression info environment)))
          (t
           ;; The form must be a compound form where the CAR is a lambda
           ;; expression.  Evaluating such a form might have some
           ;; compile-time side effects, so we must check whether we are
           ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
           ;; the form as well.
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (cst-eval client cooked-expression environment))
           (convert-lambda-call client cooked-expression environment)))))

(defmethod convert :around (client cooked-expression environment)
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (restart-case
      ;; We bind these only here so that if a restart is invoked,
      ;; the new CONVERT call will get the right values
      ;; (i.e., the ones outside our binding)
      (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
            (*subforms-are-top-level-p* nil)
            (*origin* (c:origin cooked-expression)))
        (call-next-method))
    (continue ()
      :report "Replace with call to ERROR."
      (convert client
               (c:cook
                `(error 'run-time-program-error
                        :expr ',(c:raw cooked-expression)
                        :origin ',(c:origin cooked-expression)))
               environment))
    (substitute-expression (cooked-expression)
      :report "Compile the given expression in place of the problematic one."
      (convert client cooked-expression environment))))
