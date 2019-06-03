(cl:in-package #:sicl-clos)

(defun create-method-lambda (function-name lambda-expression environment)
  (declare (ignore function-name environment))
  (make-method-lambda-default nil nil lambda-expression nil))
