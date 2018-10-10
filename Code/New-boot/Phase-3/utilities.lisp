(cl:in-package #:sicl-new-boot-phase-3)

;;; Define a function that, when called, signals an error with the
;;; name of the function in the error message.  We use this to break
;;; dependencies between loaded files.
(defun define-error-function (function-name environment)
  (setf (sicl-genv:fdefinition function-name environment)
        (lambda (&rest arguments)
          (error "Undefined function ~s called with arguments ~s."
                 function-name
                 arguments))))

;;; Some utilities for making interactive queries easier.

(define-symbol-macro ee0 (sicl-new-boot:e0 *b*))
(define-symbol-macro ee1 (sicl-new-boot:e1 *b*))
(define-symbol-macro ee2 (sicl-new-boot:e2 *b*))
(define-symbol-macro ee3 (sicl-new-boot:e3 *b*))
(define-symbol-macro ee4 (sicl-new-boot:e4 *b*))

(defun fd (function-name environment)
  (sicl-genv:fdefinition function-name environment))

(defun fbp (function-name environment)
  (sicl-genv:fboundp function-name environment))

(defun fc (class-name environment)
  (sicl-genv:find-class class-name environment))

(defun repl (environment)
  (sicl-minimal-extrinsic-environment::repl environment environment nil))
