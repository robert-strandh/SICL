(cl:in-package #:sicl-method-combination)

(defun long-form-lambda
    (lambda-list-variables method-group-specifiers declarations body)
  (let ((method-list-var (gensym "method-list")))
    `(lambda (,method-list-var ,@lambda-list-variables)
       ,@declarations
       ,(wrap-body method-list-var method-group-specifiers body))))
       
;;; We do not support the :ARGUMENTS and :GENERIC-FUNCTION options
;;; yet.
(defun long-form-expander
    (environment name lambda-list method-group-specifiers body)
  (let ((lambda-list-variables (lambda-list-variables lambda-list)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      `(let ((template (sicl-genv:find-method-combination-template
                        ',name (sicl-genv:global-environment))))
         (when (null template)
           (setf template
                 (make-instance 'method-combination-template
                   :name ',name
                   :documentation ,documentation
                   :variant-signature-determiner
                   (lambda ,lambda-list-variables
                     (list ,@lambda-list-variables))
                   :effective-method-form-function
                   ,(long-form-lambda
                     lambda-list-variables
                     method-group-specifiers
                     declarations
                     forms)))
           (setf (sicl-genv:find-method-combination-template
                  ',name (sicl-genv:global-environment))
                 template))
         ',name))))
