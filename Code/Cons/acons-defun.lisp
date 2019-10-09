(cl:in-package #:sicl-cons)

(defun acons (key datum alist)
  (cons (cons key datum) alist))
