(cl:in-package #:cleavir-cst-to-ast)

;;; During the conversion of a single CST, the value of this variable
;;; is the source location of that CST.
(defvar *origin*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT is responsible for converting a concrete syntax tree to an
;;; abstract syntax tree.

(defmethod convert (client cst lexical-environment)
  (let ((form (cst:raw cst)))
    (cond ((or (and (not (consp form)) (not (symbolp form)))
               (keywordp form)
               (member form '(t nil)))
           (convert-constant client cst lexical-environment))
          ((symbolp form)
           (convert-variable client cst lexical-environment))
          ((symbolp (car form))
           ;; Even if we are in COMPILE-TIME-TOO mode, at this point, we
           ;; do not know whether to evaluate the form at compile time,
           ;; simply because it might be a special form that is handled
           ;; specially.  So we must wait until we have more
           ;; information.
           (let ((info (describe-function client lexical-environment (car form))))
             (convert-cst client cst info lexical-environment)))
          (t
           ;; The form must be a compound form where the CAR is a lambda
           ;; expression.  Evaluating such a form might have some
           ;; compile-time side effects, so we must check whether we are
           ;; in COMPILE-TIME-TOO mode, in which case we must evaluate
           ;; the form as well.
           (when (and *current-form-is-top-level-p* *compile-time-too*)
             (cst-eval client cst lexical-environment))
           (convert-lambda-call client cst lexical-environment)))))

(defmethod convert :around (client cst lexical-environment)
  (restart-case
      ;; We bind these only here so that if a restart is invoked,
      ;; the new CONVERT call will get the right values
      ;; (i.e., the ones outside our binding)
      (let ((*current-form-is-top-level-p* *subforms-are-top-level-p*)
            (*subforms-are-top-level-p* nil)
            (*origin* (cst:source cst)))
        (call-next-method))
    (continue ()
      :report "Replace with call to ERROR."
      (convert client
               (cst:cst-from-expression
                `(error 'run-time-program-error
                        :expr ',(cst:raw cst)
                        :origin ',(cst:source cst)))
               lexical-environment))
    (substitute-cst (cst)
      :report "Compile the given CST in place of the problematic one."
      (convert client cst lexical-environment))))
