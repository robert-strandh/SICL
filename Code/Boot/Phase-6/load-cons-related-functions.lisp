(cl:in-package #:sicl-boot-phase-6)

(defun load-cons-related-functions (e5)
  (load-fasl "Cons/null-defun.fasl" e5)
  (load-fasl "Cons/list-defun.fasl" e5)
  (load-fasl "Cons/list-star-defun.fasl" e5)
  (load-fasl "Cons/make-list-defun.fasl" e5)
  (load-fasl "Cons/set-difference-defun.fasl" e5)
  (load-fasl "Cons/endp-defun.fasl" e5))
