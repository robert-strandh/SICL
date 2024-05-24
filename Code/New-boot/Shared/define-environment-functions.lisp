(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

(defun define-environment-functions (client global-environment)
  (flet ((import-clostrum-function (clostrum-function sicl-environment-name)
           (setf (clo:fdefinition
                  client global-environment sicl-environment-name)
                 clostrum-function)))
    (let ((items (list (cons #'clo:find-class
                             @clostrum:find-class)
                       (cons #'(setf clo:find-class)
                             (list 'setf @clostrum:find-class))
                       (cons #'clo:fboundp
                             @clostrum:fboundp)
                       (cons #'clo:fdefinition
                             @clostrum:fdefinition)
                       (cons #'(setf clo:fdefinition)
                             (list 'setf @clostrum:fdefinition))
                       (cons #'clo:macro-function
                             @clostrum:macro-function)
                       (cons #'(setf clo:macro-function)
                             (list 'setf @clostrum:macro-function))
                       (cons #'clo:compiler-macro-function
                             @clostrum:compiler-macro-function)
                       (cons #'(setf clo:compiler-macro-function)
                             (list 'setf @clostrum:compiler-macro-function))
                       (cons #'clo:make-constant
                             @clostrum:make-constant))))
      (loop for (function . name) in items
            do (import-clostrum-function function name))))
  (setf (clo:fdefinition client global-environment '(setf fdefinition))
        (lambda (new-definition name &key (environment global-environment))
          (setf (clo:fdefinition client environment name)
                new-definition)))
  (let ((environment
          (make-instance 'trucler-reference:environment
            :global-environment global-environment)))
    (ensure-asdf-system client environment "sicl-environment-shared"))
  (setf (clo:fdefinition client global-environment 'boundp)
        (lambda (name &key (environment global-environment))
          (env:boundp name :environment environment)))
  (setf (clo:fdefinition client global-environment 'symbol-value)
        (lambda (name &key (environment global-environment))
          (env:symbol-value name :environment environment)))
  (setf (clo:fdefinition client global-environment '(setf symbol-value))
        (lambda (new-value name &key (environment global-environment))
          (setf (env:symbol-value name :environment environment)
                new-value)))
  (let ((symbol-define-constant @sicl-environment:define-constant))
    (setf (clo:macro-function client global-environment 'defconstant)
          (lambda (form environment)
            (declare (ignore environment))
            (destructuring-bind (name value-form &optional documentation)
                (rest form)
              (declare (ignore documentation))
              `(,symbol-define-constant ',name ,value-form))))))
