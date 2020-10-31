(cl:in-package #:sicl-method-combination)

(defun define-method-combination-expander (name more-arguments)
  (if (or (null more-arguments)
          (and (symbolp (first more-arguments))
               (not (null (first more-arguments)))))
      (short-form-expander name more-arguments)
      (destructuring-bind (lambda-list method-group-specifiers . body)
          more-arguments
        (long-form-expander name
                            lambda-list
                            method-group-specifiers
                            body))))
