(cl:in-package #:sicl-new-boot-phase-1)

(defun load-ctype (client environment global-environment)
  ;; The ctype library defines a method combination.  In phase 1, this
  ;; needs to be a host method combination, but it needs to be
  ;; available to DEFGENERIC.  We have programmed the DEFGENERIC macro
  ;; to search for method combinations in the host, so there is
  ;; nothing else we need to do.
  (setf (clo:macro-function
         client global-environment 'define-method-combination)
        (lambda (form environment)
          (declare (ignore environment))
          (eval form)
          nil))
  ;; The ctype library needs for the system SICL-ARITHMETIC to be
  ;; loaded.
  ;; I have no idea why this is necessary.
  (let ((symbol (find-symbol "LIST-STRUCTURE" "ECCLESIA")))
    (setf (clo:fdefinition client global-environment symbol)
          (fdefinition symbol)))
  (sb:ensure-asdf-system client environment "sicl-arithmetic-base")
  ;; The ctype library needs for the classes in the module SICL-ARRAY
  ;; to be defined.
  (sb:ensure-asdf-system client environment "sicl-array-support")
  ;; The SICL-ARRAY module defines vector to have SEQUENCE as a
  ;; superclass, but it doesn't have to work in this phase, so we
  ;; define SEQUENCE to be the host class T.
  (setf (clo:find-class client global-environment 'sequence)
        (find-class 't))
  (sb:ensure-asdf-system client environment "sicl-array-load-time")
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system client environment "ctype")))
