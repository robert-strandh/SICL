(cl:in-package #:sicl-hir-evaluator-test)

(defclass environment (sicl-extrinsic-environment:environment)
  ())

(defun foo (x)
  (declare (ignore x))
  nil)

(defun bar (x)
  (declare (ignore x))
  42)

(defparameter *x* 5)

(defun import-function-from-host (function-name environment)
  (setf (sicl-genv:fdefinition function-name environment)
        (fdefinition function-name)))

(defun make-environment ()
  (let ((environment (make-instance 'environment)))
    (sicl-extrinsic-environment::import-from-host environment)
    (loop for symbol in '(list 1+ 1- zerop plusp) do
      (import-function-from-host symbol environment))
    (setf (sicl-genv:fdefinition 'enclose environment)
          #'sicl-hir-evaluator:enclose)
    (setf (sicl-genv:fdefinition 'symbol-value environment)
          (sicl-hir-evaluator:symbol-value-function environment))
    (setf (sicl-genv:fdefinition '(setf symbol-value) environment)
          (sicl-hir-evaluator:set-symbol-value-function environment))
    ;; Treat all primops as special.
    (loop for symbol being each external-symbol of "CLEAVIR-PRIMOP" do
      (setf (sicl-genv:special-operator symbol environment)
            t))
    ;; Define a few auxiliary functions and variables for testing.
    (setf (sicl-genv:fdefinition 'foo environment)
          #'foo)
    (setf (sicl-genv:fdefinition 'bar environment)
          #'bar)
    (setf (sicl-genv:special-variable '*x* environment t)
          *x*)
    environment))
