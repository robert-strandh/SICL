(cl:in-package #:sicl-evaluation-and-compilation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro LAMBDA.

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))
