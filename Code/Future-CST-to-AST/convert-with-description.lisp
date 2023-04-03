(cl:in-package #:sicl-expression-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a symbol macro.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:symbol-macro-description)
     environment)
  (let* ((expansion (trucler:expansion description))
         (expander (symbol-macro-expander expansion))
         (raw-expanded-form
           (expand-macro expander cooked-form environment))
         (cooked-expanded-form
           (c:reconstruct raw-expanded-form cooked-form)))
    (setf (c:origin cooked-expanded-form) (c:origin cooked-form))
    (with-preserved-toplevel-ness
      (convert client cooked-expanded-form environment))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a constant variable.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:constant-variable-description)
     environment)
  (let ((new-cooked-form (c:cook (trucler:value description))))
    (setf (c:origin new-cooked-form) (c:origin cooked-form))
    (convert-constant client new-cooked-form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a cooked special form.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:special-operator-description)
     environment)
  (let ((builder (make-builder client environment)))
    (s-expression-syntax:parse builder t cooked-form)))

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

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:local-macro-description)
     environment)
  (let* ((expander (trucler:expander description))
         (raw-expanded-form
           (expand-macro expander cooked-form environment))
         (cooked-expanded-form
           (c:reconstruct raw-expanded-form cooked-form)))
    (setf (c:origin cooked-expanded-form) (c:origin cooked-form))
    (with-preserved-toplevel-ness
      (convert client cooked-expanded-form environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a compound form that calls a global macro.
;;; A global macro can have a compiler macro associated with it.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:global-macro-description) environment)
  (let ((compiler-macro (trucler:compiler-macro description))
        (expander (trucler:expander description)))
    (with-preserved-toplevel-ness
      (if (null compiler-macro)
          ;; There is no compiler macro, so we just apply the macro
          ;; expander, and then convert the resulting form.
          (let* ((raw-expanded-form
                   (expand-macro expander cooked-form environment))
                 (cooked-expanded-form
                   (c:reconstruct raw-expanded-form cooked-form)))
            (setf (c:origin cooked-expanded-form) (c:origin cooked-form))
            (convert client cooked-expanded-form environment))
          ;; There is a compiler macro, so we must see whether it will
          ;; accept or decline.
          (let ((raw-expanded-form
                  (expand-compiler-macro
                   compiler-macro cooked-form environment)))
            (if (eq (c:raw cooked-form) raw-expanded-form)
                ;; If the two are EQ, this means that the compiler macro
                ;; declined.  Then we appply the macro function, and
                ;; then convert the resulting form, just like we did
                ;; when there was no compiler macro present.
                (let* ((raw-expanded-form
                         (expand-macro expander cooked-form environment))
                       (cooked-expanded-form
                         (c:reconstruct raw-expanded-form cooked-form)))
                  (setf (c:origin cooked-expanded-form)
                        (c:origin cooked-form))
                  (convert client cooked-expanded-form environment))
                ;; If the two are not EQ, this means that the compiler
                ;; macro replaced the original form with a new form.
                ;; This new form must then again be converted without
                ;; taking into account the real macro expander.
                (let ((cooked-expanded-form
                        (c:reconstruct raw-expanded-form cooked-form)))
                  (setf (c:origin cooked-expanded-form)
                        (c:origin cooked-form))
                  (convert client cooked-expanded-form environment))))))))

(defun convert-arguments (client cooked-arguments environment)
  (if (c:null cooked-arguments)
      c:nil
      (c:cons
       (convert client (c:first cooked-arguments) environment)
       (convert-arguments client (c:rest cooked-arguments) environment))))

(defun make-application (client cooked-form environment)
  (make-instance 'ico:application-ast
    :origin (c:origin cooked-form)
    :function-ast
    (make-instance 'ico:function-name-ast
      :origin (c:origin (c:first cooked-form))
      :name (c:first cooked-form))
    :argument-asts
    (convert-arguments client (c:rest cooked-form) environment)))

;;; Convert a form representing a call to a named global function.
(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:global-function-description)
     environment)
  ;; When we compile a call to a global function, it is possible that
  ;; we are in COMPILE-TIME-TOO mode.  In that case, we must first
  ;; evaluate the form.
  (when (and *current-form-is-top-level-p* *compile-time-too*)
    (eval-cooked client cooked-form environment))
  (let ((compiler-macro (trucler:compiler-macro description))
        (notinline (eq 'notinline (trucler:inline description))))
    (if (or notinline (null compiler-macro))
        ;; There is no compiler macro.  Create the application.
        (make-application client cooked-form environment)
        ;; There is a compiler macro.  We must see whether it will
        ;; accept or decline.
        (let ((raw-expanded-form
                (expand-compiler-macro
                 compiler-macro cooked-form environment)))
          (if (eq (c:raw cooked-form) raw-expanded-form)
              ;; If the two are EQ, this means that the compiler macro
              ;; declined.  We are left with function-call form.
              ;; Create the application, just as if there were no
              ;; compiler macro present.
              (make-application client cooked-form environment)
              ;; If the two are not EQ, this means that the compiler
              ;; macro replaced the original form with a new form.
              ;; This new form must then be converted.
              (let ((cooked-expanded-form
                      (c:reconstruct raw-expanded-form cooked-form)))
                (setf (c:origin cooked-expanded-form) (c:origin cooked-form))
                (convert client cooked-expanded-form environment)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a cooked expression representing a compound form that
;;; calls a local function.  A local function can not have a compiler
;;; macro associated with it.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:local-function-description)
     environment)
  (make-application client cooked-form environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a special variable.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:special-variable-description)
     environment)
  (make-instance 'ico:variable-name-ast
    :origin (c:origin cooked-form)
    :name (trucler:name description)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting a symbol that has a definition as a lexical variable.

(defmethod convert-with-description
    (client
     cooked-form
     (description trucler:lexical-variable-description)
     environment)
  (let* ((variable-definition-ast (trucler:identity description))
         (result (make-instance 'ico:variable-reference-ast
                   :origin (c:origin cooked-form)
                   :name (trucler:name description)
                   :variable-definition-ast variable-definition-ast)))
    (reinitialize-instance variable-definition-ast
      :variable-reference-asts
      (append (ico:variable-reference-asts variable-definition-ast)
              (list result)))
    result))
