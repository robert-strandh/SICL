(cl:in-package #:sicl-boot-phase-4)

(defun import-code-utilities (e4)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:canonicalize-generic-function-lambda-list
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-specialized-lambda-list
     cleavir-code-utilities:separate-function-body)
   e4))

(defun import-misc (e4)
  (import-functions-from-host
   '(sicl-method-combination:define-method-combination-expander
     coerce)
   e4)
  (setf (env:macro-function (env:client e4) e4 'defpackage)
        (lambda (form env)
          (declare (ignore env))
          (eval form)
          nil)))

(defun import-data-and-control-flow (e4)
  (import-functions-from-host
   '(not eq eql equal values)
   e4))

(defun import-from-host (e4)
  (import-code-utilities e4)
  (import-misc e4)
  (import-data-and-control-flow e4))
