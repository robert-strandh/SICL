(cl:in-package #:sicl-extrinsic-environment)

(defun fill-environment (environment)
  (import-from-host-common-lisp environment)
  (define-defmacro environment)
  (define-default-setf-expander environment))
