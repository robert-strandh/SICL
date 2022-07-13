(cl:in-package #:sicl-boot-phase-4)

(defun import-misc (e4)
  (setf (env:macro-function (env:client e4) e4 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil)))

(defun import-from-host (e4)
  (import-misc e4))
