(cl:in-package #:sicl-environment)

(defun find-class
    (class-name &optional (errorp t) (environment *environment*))
  (clo:find-class *client* environment class-name errorp))

(defun (setf find-class)
    (new-class class-name &optional (errorp t) (environment *environment*))
  (setf (clo:find-class *client* environment class-name errorp)
        new-class))
