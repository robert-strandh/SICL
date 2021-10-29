(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-cst
    (client cst (info trucler:symbol-macro-description) environment)
  (let* ((expansion (trucler:expansion info))
         (expander (symbol-macro-expander expansion))
         (expanded-form (expand-macro expander cst environment))
         (expanded-cst (cst:reconstruct expanded-form cst client)))
    (setf (cst:source expanded-cst) (cst:source cst))
    (with-preserved-toplevel-ness
      (convert client
               expanded-cst
               environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a constant variable.

(defmethod convert-cst
    (client cst (info trucler:constant-variable-description) environment)
  (let ((new-cst (cst:cst-from-expression (trucler:value info))))
    (setf (cst:source new-cst) (cst:source cst))
    (convert-constant client new-cst environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a special form represented as a CST.

(defmethod convert-cst
    (client cst (info trucler:special-operator-description) environment)
  (convert-special client
                   (car (cst:raw cst))
                   cst
                   environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a local macro.
;;; A local macro can not have a compiler macro associated with it.
;;;
;;; If we found a local macro in ENVIRONMENT, it means that
;;; ENVIRONMENT is not the global environment.  And it must be
;;; the same kind of agumentation environment that was used when the
;;; local macro was created by the use of MACROLET.  Therefore, the
;;; expander should be able to handle being passed the same kind of
;;; environment.

(defmethod convert-cst
    (client cst (info trucler:local-macro-description) environment)
  (let* ((expander (trucler:expander info))
         (expanded-form (expand-macro expander cst environment))
         (expanded-cst (cst:reconstruct expanded-form cst client)))
    (setf (cst:source expanded-cst) (cst:source cst))
    (with-preserved-toplevel-ness
      (convert client expanded-cst environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

(defmethod convert-cst
    (client cst (info trucler:global-macro-description) environment)
  (let ((compiler-macro (trucler:compiler-macro info))
        (expander (trucler:expander info)))
    (with-preserved-toplevel-ness
      (if (null compiler-macro)
          ;; There is no compiler macro, so we just apply the macro
          ;; expander, and then convert the resulting form.
          (let* ((expanded-form (expand-macro expander cst environment))
                 (expanded-cst (cst:reconstruct expanded-form cst client)))
            (setf (cst:source expanded-cst) (cst:source cst))
            (convert client expanded-cst environment))
          ;; There is a compiler macro, so we must see whether it will
          ;; accept or decline.
          (let ((expanded-form
                  (expand-compiler-macro compiler-macro cst environment)))
            (if (eq (cst:raw cst) expanded-form)
                ;; If the two are EQ, this means that the compiler macro
                ;; declined.  Then we appply the macro function, and
                ;; then convert the resulting form, just like we did
                ;; when there was no compiler macro present.
                (let* ((expanded-form
                         (expand-macro expander cst environment))
                       (expanded-cst (cst:reconstruct expanded-form cst client)))
                  (setf (cst:source expanded-cst) (cst:source cst))
                  (convert client expanded-cst environment))
                ;; If the two are not EQ, this means that the compiler
                ;; macro replaced the original form with a new form.
                ;; This new form must then again be converted without
                ;; taking into account the real macro expander.
                (let ((expanded-cst (cst:reconstruct expanded-form cst client)))
                  (setf (cst:source expanded-cst) (cst:source cst))
                  (convert client expanded-cst environment))))))))

;;; Construct a CALL-AST representing a function-call form.  CST is
;;; the concrete syntax tree representing the entire function-call
;;; form.  ARGUMENTS-CST is a CST representing the sequence of
;;; arguments to the call.
(defun make-call (client cst info environment arguments-cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (let* ((name-cst (cst:first cst))
         (function-ast
           (convert-called-function-reference client name-cst info environment))
         (argument-asts
           (convert-sequence client arguments-cst environment)))
    (cleavir-ast:make-ast 'cleavir-ast:call-ast
     :callee-ast function-ast
     :argument-asts argument-asts)))

(defun make-global-call (client cst info environment argument-cst)
  (declare (ignore info))
  (cleavir-ast:make-ast 'cleavir-ast:named-call-ast
    :callee-name (cst:raw (cst:first cst))
    :argument-asts (convert-sequence client argument-cst environment)))

;;; Convert a form representing a call to a named global function.
;;; CST is the concrete syntax tree representing the entire
;;; function-call form.  INFO is the info instance returned form a
;;; query of the environment with the name of the function.
(defmethod convert-cst
    (client cst (info trucler:global-function-description) environment)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (cst-eval client cst environment))
  (let ((compiler-macro (trucler:compiler-macro info))
        (notinline (eq 'notinline (trucler:inline info))))
    (if (or notinline (null compiler-macro))
        ;; There is no compiler macro.  Create the call.
        (make-global-call client cst info environment (cst:rest cst))
        ;; There is a compiler macro.  We must see whether it will
        ;; accept or decline.
        (let ((expanded-form
                (expand-compiler-macro compiler-macro cst environment)))
          (if (eq (cst:raw cst) expanded-form)
              ;; If the two are EQ, this means that the compiler macro
              ;; declined.  We are left with function-call form.
              ;; Create the call, just as if there were no compiler
              ;; macro present.
              (make-global-call client cst info environment (cst:rest cst))
              ;; If the two are not EQ, this means that the compiler
              ;; macro replaced the original form with a new form.
              ;; This new form must then be converted.
              (let ((expanded-cst (cst:reconstruct expanded-form cst client)))
                (setf (cst:source expanded-cst) (cst:source cst))
                (convert client expanded-cst environment)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a CST representing a compound form that calls a local
;;; function.  A local function can not have a compiler macro
;;; associated with it.

(defmethod convert-cst
    (client cst (info trucler:local-function-description) environment)
  (make-call client cst info environment (cst:rest cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a special variable.
;;; We do this by generating a SYMBOL-VALUE-AST.

(defmethod convert-special-variable (client cst info global-environment)
  (declare (ignore global-environment))
  (let ((name-ast (cleavir-ast:make-ast 'cleavir-ast:constant-ast
                    :value (trucler:name info))))
    (cleavir-ast:make-ast 'cleavir-ast:symbol-value-ast
      :name-ast name-ast)))

(defmethod convert-cst
    (client cst (info trucler:special-variable-description) environment)
  (let ((global-environment (trucler:global-environment client environment)))
    (convert-special-variable client cst info global-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a lexical variable.

(defmethod convert-cst
    (client cst (info trucler:lexical-variable-description) environment)
  (declare (ignore client))
  (when (eq (trucler:ignore info) 'ignore)
    (warn 'ignored-variable-referenced :cst cst))
  (trucler:identity info))
