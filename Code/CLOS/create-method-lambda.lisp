(cl:in-package #:sicl-clos)

(defun create-method-lambda
    (possible-generic-function lambda-expression environment)
  (let ((generic-function-p
          (typep possible-generic-function 'generic-function)))
    (make-method-lambda
     (if generic-function-p
         possible-generic-function
         (class-prototype
          (sicl-genv:find-class 'standard-generic-function environment)))
     (class-prototype
      (if generic-function-p
          (generic-function-method-class possible-gener)
          (sicl-genv:find-class 'standard-method ct-env)))
     lambda-expression
     environment)))
