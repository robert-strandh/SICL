(cl:in-package #:sicl-method-combination)

(defun long-form-lambda (lambda-list method-group-specifiers declarations body)
  (let ((lambda-list-variables (lambda-list-variables lambda-list)))
    `(lambda ,lambda-list-variables
       ,declarations
       ,(wrap-body method-group-specifiers body))))
       
;;; We do not support the :ARGUMENTS and :GENERIC-FUNCTION options
;;; yet.
(defun long-form-expander
    (environment name lambda-list method-group-specifiers body)
  (multiple-value-bind (declarations documentation forms)
      (cleavir-code-utilities:separate-function-body body)
    `(setf (sicl-genv:find-method-combination-template ',name ,environment)
           (make-instance 'method-combination-template
             :name ',name
             :documentation ,documentation
             :effective-method-form-function
             ,(long-form-lambda
               lambda-list method-group-specifiers declarations forms)))))
