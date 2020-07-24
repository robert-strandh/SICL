(cl:in-package #:target-evaluation-and-compilation)

(defmacro lambda (lambda-list &body body)
  `(function (lambda ,lambda-list ,@body)))
