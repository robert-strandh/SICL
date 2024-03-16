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
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system client environment "ctype")))
