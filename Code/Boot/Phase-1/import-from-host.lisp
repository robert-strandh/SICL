(cl:in-package #:sicl-boot-phase-1)

(defun import-trucler-functions (environment)
  (import-functions-from-host
   '(trucler:symbol-macro-expansion
     trucler:macro-function)
   environment))

(defun import-misc (environment)
  (import-functions-from-host
   '(error typep coerce)
   environment))

(defun import-from-host (environment)
  (import-trucler-functions environment)
  (import-misc environment))

(defun define-defgeneric-expander (client environment)
  (setf (env:fdefinition client environment 'sicl-clos:defgeneric-expander)
        (lambda (name lambda-list options-and-methods environment)
          (declare (ignore environment))
          (assert (or (null options-and-methods)
                      (and (null (cdr options-and-methods))
                           (eq (caar options-and-methods)
                               :argument-precedence-order))))
          `(ensure-generic-function
            ',name
            :lambda-list ',lambda-list
            ,@(if (null options-and-methods)
                  '()
                  `(:argument-precedence-order ',(cdar options-and-methods)))
            :environment (env:global-environment)))))
