(cl:in-package #:sicl-expression-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a cooked form to an abstract
;;; syntax tree.

(defmethod convert (client cooked-form environment)
  (let ((raw-form (c:raw cooked-form)))
    (cond ((or (and (not (consp raw-form)) (not (symbolp raw-form)))
               (keywordp raw-form)
               (member raw-form '(t nil)))
           (convert-constant client cooked-form environment))
          ((symbolp raw-form)
           (convert-variable client cooked-form environment))
          ((symbolp (car raw-form))
           ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
           ;; do not know whether to evaluate the form at compile time,
           ;; simply because it might be a special form that is handled
           ;; specially.  So we must wait until we have more
           ;; information.
           (let* ((operator (c:first cooked-form))
                  (info (describe-function client environment operator)))
             (convert-with-description
              client cooked-form info environment)))
          (t
           ;; The form must be a compound form where the CAR is a lambda
           ;; expression.  Evaluating such a form might have some
           ;; compile-time side effects, so we must check whether we are
           ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
           ;; the form as well.
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (eval-cooked client cooked-form environment))
           (convert-lambda-call client cooked-form environment)))))

(defmethod convert :around (client cooked-form environment)
  #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (restart-case
      ;; We bind these only here so that if a restart is invoked,
      ;; the new CONVERT call will get the right values
      ;; (i.e., the ones outside our binding)
      (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
            (*subforms-are-top-level-p* nil)
            (*origin* (c:origin cooked-form)))
        (call-next-method))
    (continue ()
      :report "Replace with call to ERROR."
      (convert client
               (c:cook
                `(error 'run-time-program-error
                        :expr ',(c:raw cooked-form)
                        :origin ',(c:origin cooked-form)))
               environment))
    (substitute-form (cooked-form)
      :report "Compile the given form in place of the problematic one."
      (convert client cooked-form environment))))
