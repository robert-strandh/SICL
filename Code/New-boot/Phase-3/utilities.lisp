(cl:in-package #:sicl-new-boot-phase-3)

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

;;; Define a function that, when called, signals an error with the
;;; name of the function in the error message.  We use this to break
;;; dependencies between loaded files.
(defun define-error-function (function-name environment)
  (setf (sicl-genv:fdefinition function-name environment)
        (lambda (&rest arguments)
          (error "Undefined function ~s called with arguments ~s."
                 function-name
                 arguments))))
