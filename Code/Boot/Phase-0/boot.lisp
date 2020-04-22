(cl:in-package #:sicl-boot-phase-0)

;;; During bootstrapping, we compile a certain number of files
;;; containing DEFUN forms.  The macro expander for DEFUN is executed
;;; by the host, because it itself is defined using DEFUN.  That
;;; microsecond calls SICL-GENV:GLOBAL-ENVIRONMENT, so that function
;;; must be defined in the host.  For that reason, we define it here.
(defun sicl-genv:global-environment (&optional environment)
  (assert (typep environment 'environment))
  environment)

(defun boot (boot)
  (format *trace-output* "Start phase 0~%")
  (with-accessors ((e0 sicl-boot:e0)) boot
    (change-class e0 'environment)
    (let ((client (sicl-genv:client e0)))
      (fill-environment client e0)
      (compile-files client e0))))
