(cl:in-package #:sicl-boot-phase-5)

(defun import-misc (e5)
  (import-functions-from-host
   '(coerce)
   e5)
  (setf (env:macro-function (env:client e5) e5 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil))
  ;; Fake PROGV for now.
  (setf (env:macro-function (env:client e5) e5 'progv)
        (lambda (form env)
          (declare (ignore env))
          (cons 'progn (rest form)))))

(defun import-from-host (e5)
  (import-misc e5))
