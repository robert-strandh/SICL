(cl:in-package #:sicl-boot-phase-0)

(defun fill-environment (client environment)
  (declare (ignore client))
  (import-from-host environment)
  (define-defmacro environment)
  (define-backquote-macros environment)
  (define-default-setf-expander environment))
