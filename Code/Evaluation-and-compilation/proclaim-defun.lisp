(cl:in-package #:sicl-evaluation-and-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PROCLAIM.

(symbol-macrolet ((client sicl-client:*client*))
  (let* ((environment (env:global-environment))
         (proclamation-function
           (fdefinition 'env:proclamation))
         (setf-proclamation-function
           (fdefinition '(setf env:proclamation))))

    (defun declaration-proclamations ()
      (funcall proclamation-function client environment 'declaration))

    ;; FIXME: check that NAMES is a proper list.
    (defun proclaim-declarations (names)
      (funcall setf-proclamation-function
               (append names (declaration-proclamations))
               client environment 'declaration))))

(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (proclaim-declarations (cdr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))
