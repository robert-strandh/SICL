(cl:in-package #:sicl-new-boot)

(defun load-file (file environment)
  (sicl-minimal-extrinsic-environment:cst-load-file file environment nil))

;;; Function LOAD-FILE-PROTECTED (protected loading).  It wraps the
;;; loading of a file in a handler that invokes a restart that tells
;;; the compiler to treat all undefined functions as if they were
;;; global.  This function should only be used in exceptional cases
;;; because it is better to find undefined functions at compile time
;;; than at run time.  However, sometimes we need it.  In particular,
;;; since Cleavir is currently not smart enough to recognize
;;; self-recursive functions, we need this function in those
;;; situations.
(defun load-file-protected (file environment)
  (handler-bind
      ((cleavir-env:no-function-info
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'cleavir-cst-to-ast:consider-global))))
    (load-file file environment)))

(defun import-function-from-host (name environment)
  (sicl-minimal-extrinsic-environment:import-function-from-host name environment))

(defun import-functions-from-host (names environment)
  (loop for name in names
        do (import-function-from-host name environment)))

(defun import-package-from-host (name environment)
  (sicl-minimal-extrinsic-environment:import-package-from-host name environment))

(defun import-class-from-host (name environment)
  (setf (sicl-genv:find-class name environment)
        (find-class name)))

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
(define-symbol-macro ee5 (sicl-new-boot:e5 *b*))

(defun fd (function-name environment)
  (sicl-genv:fdefinition function-name environment))

(defun fbp (function-name environment)
  (sicl-genv:fboundp function-name environment))

(defun fc (class-name environment)
  (sicl-genv:find-class class-name environment))

(defun repl (environment)
  (sicl-minimal-extrinsic-environment::repl environment environment nil))
