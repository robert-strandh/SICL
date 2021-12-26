(cl:in-package #:sicl-boot-phase-5)

(defun pre-fill-environment (e5)
  (define-error-functions
      '(nreconc)
      e5))
