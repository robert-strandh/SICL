(cl:in-package #:sicl-boot-phase-6)

(defun load-cons-related-functions (e5)
  (load-fasl "Cons/null-defun.fasl" e5))
