(cl:in-package #:sicl-environment)

(defun macro-function (macro-name &optional (environment *environment*))
  (clo:macro-function *client* environment macro-name))

(defun (setf macro-function)
    (new-macro-function macro-name &optional (environment *environment*))
  (setf (clo:macro-function *client* environment macro-name)
        new-macro-function))
