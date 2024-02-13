(cl:in-package #:sicl-environment)

;;; I am not sure whether the standard allows the implementation to
;;; add keyword arguments to standard functions that don't already
;;; have some.  But for now, I am going to assume that it is allowed.

(defun fdefinition (function-name &key (environment *environment*))
  (clo:fdefinition *client* environment function-name))

(defun (setf fdefinition)
    (new-definition function-name &key (environment *environment*))
  (setf (clo:fdefinition *client* environment function-name)
        new-definition))
