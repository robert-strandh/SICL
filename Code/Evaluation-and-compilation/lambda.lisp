(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro LAMBDA.

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))


