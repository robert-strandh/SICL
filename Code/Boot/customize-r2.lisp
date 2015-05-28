(cl:in-package #:sicl-boot)

(defun customize-r2 (boot)
  (define-make-instance boot)
  (define-direct-slot-definition-class boot)
  (define-find-class boot))

