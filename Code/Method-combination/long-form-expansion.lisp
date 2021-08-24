(cl:in-package #:sicl-method-combination)

(defun long-form-lambda
    (lambda-list-variables method-group-specifiers declarations body)
  (let ((method-list-var (gensym "method-list")))
    `(lambda (,method-list-var ,@lambda-list-variables)
       ,@declarations
       ,(wrap-body method-list-var method-group-specifiers body))))
       
;;; We do not support the :ARGUMENTS and :GENERIC-FUNCTION options
;;; yet.
(defun long-form-expander (name lambda-list method-group-specifiers body)
  (let ((lambda-list-variables
          (cleavir-code-utilities:lambda-list-variables lambda-list)))
    (multiple-value-bind (declarations documentation forms)
        (cleavir-code-utilities:separate-function-body body)
      `(let ((template (find-method-combination-template ',name)))
         (when (null template)
           (setf template
                 (make-instance 'method-combination-template
                   :name ',name
                   :documentation ,documentation
                   :variant-signature-determiner
                   (lambda ,lambda-list
                     (list ,@lambda-list-variables))
                   :effective-method-form-function
                   ,(long-form-lambda
                     lambda-list-variables
                     method-group-specifiers
                     declarations
                     forms)))
           (setf (find-method-combination-template ',name)
                 template))
         ',name))))
