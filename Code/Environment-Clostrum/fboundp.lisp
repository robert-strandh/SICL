(cl:in-package #:sicl-environment)

;;; I am not sure whether the standard allows the implementation to
;;; add keyword arguments to standard functions that don't already
;;; have some.  But for now, I am going to assume that it is allowed.

(defun fboundp (function-name &key (environment *environment*))
  (clo:fboundp *client* environment function-name))
