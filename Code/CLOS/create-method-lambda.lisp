(cl:in-package #:sicl-clos)

(defun create-method-lambda
    (function-or-nil lambda-expression environment)
  (make-method-lambda
   (if (null function-or-nil)
       (class-prototype
        (sicl-genv:find-class 'standard-generic-function ct-env))
       function-or-nil)
   (class-prototype
    (if (null function-or-nil)
        (sicl-genv:find-class 'standard-method ct-env)
        (generic-function-method-class fun)))
   lambda-expression
   environment))
