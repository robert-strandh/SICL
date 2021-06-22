(cl:in-package #:sicl-evaluation-and-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PROCLAIM.

(let* ((environment (sicl-environment:global-environment))
       (client (sicl-environment:client environment))
       (proclamation-function
         (fdefinition 'sicl-environment:proclamation))
       (setf-proclamation-function
         (fdefinition '(setf sicl-environment:proclamation))))

  (defun declaration-proclamations ()
    (funcall proclamation-function client environment 'declaration))

  ;; FIXME: check that NAMES is a proper list.
  (defun proclaim-declarations (names)
    (funcall setf-proclamation-function
             (append names (declaration-proclamations))
             client environment 'declaration)))

(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (proclaim-declarations (cdr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))
