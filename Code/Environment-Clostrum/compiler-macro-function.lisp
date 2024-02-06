(cl:in-package #:sicl-environment)

(defun compiler-macro-function
    (macro-name &optional (environment *environment*))
  (clo:compiler-macro-function *client* environment macro-name))

(defun (setf compiler-macro-function)
    (new-function macro-name &optional (environment *environment*))
  (setf (clo:compiler-macro-function *client* environment macro-name)
        new-function))
