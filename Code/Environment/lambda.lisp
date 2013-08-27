(cl:in-package #:sicl-compiler-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro LAMBDA.

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))


