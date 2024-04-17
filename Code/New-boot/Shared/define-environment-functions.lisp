(cl:in-package #:sicl-new-boot)

(eval-when (:compile-toplevel) (enable-parcl-symbols client))

(defun define-environment-functions (client global-environment)
  (flet ((import-clostrum-function (clostrum-function sicl-environment-name)
           (setf (clo:fdefinition
                  client global-environment sicl-environment-name)
                 clostrum-function)))
    (let ((items (list (cons #'clo:find-class
                             @sicl-environment:find-class)
                       (cons #'(setf clo:find-class)
                             (list 'setf @sicl-environment:find-class))
                       (cons #'clo:fboundp
                             @sicl-environment:fboundp)
                       (cons #'clo:fdefinition
                             @sicl-environment:fdefinition)
                       (cons #'(setf clo:fdefinition)
                             (list 'setf @sicl-environment:fdefinition))
                       (cons #'clo:macro-function
                             @sicl-environment:macro-function)
                       (cons #'(setf clo:macro-function)
                             (list 'setf @sicl-environment:macro-function))
                       (cons #'clo:compiler-macro-function
                             @sicl-environment:compiler-macro-function)
                       (cons #'(setf clo:compiler-macro-function)
                             (list 'setf @sicl-environment:compiler-macro-function)))))
      (loop for (function . name) in items
            do (import-clostrum-function function name))))
  (setf (clo:fdefinition client global-environment 'find-class)
        (lambda (name &optional (errorp t) (environment global-environment))
          (if (symbolp name)
              (clo:find-class client environment name errorp)
              name)))
  (setf (clo:fdefinition client global-environment '(setf find-class))
        (lambda (new-class name
                 &optional (errorp t) (environment global-environment))
          (setf (clo:find-class client environment name errorp)
                new-class)))
  (setf (clo:fdefinition client global-environment 'fboundp)
        (lambda (name &key (environment global-environment))
          (clo:fboundp client environment name)))
  (setf (clo:fdefinition client global-environment 'fdefinition)
        (lambda (name &key (environment global-environment))
          (clo:fdefinition client environment name)))
  (setf (clo:fdefinition client global-environment '(setf fdefinition))
        (lambda (new-definition name &key (environment global-environment))
          (setf (clo:fdefinition client environment name)
                new-definition)))
  (setf (clo:fdefinition client global-environment 'macro-function)
        (lambda (name &key (environment global-environment))
          (clo:macro-function client environment name)))
  (setf (clo:fdefinition client global-environment '(setf macro-function))
        (lambda (new-definition name &key (environment global-environment))
          (setf (clo:macro-function client environment name)
                new-definition)))
  (setf (clo:fdefinition client global-environment 'compiler-macro-function)
        (lambda (name &key (environment global-environment))
          (clo:compiler-macro-function client environment name)))
  (setf (clo:fdefinition
         client global-environment '(setf compiler-macro-function))
        (lambda (new-definition name &key (environment global-environment))
          (setf (clo:compiler-macro-function client environment name)
                new-definition)))
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
    (setf (clo:fdefinition
           client global-environment symbol-define-constant)
          (lambda (name initial-value
                   &key (environment global-environment))
            (env:define-constant
                name initial-value :environment environment)))
    (setf (clo:macro-function client global-environment 'defconstant)
          (lambda (form environment)
            (declare (ignore environment))
            (destructuring-bind (name value-form &optional documentation)
                (rest form)
              (declare (ignore documentation))
              `(,symbol-define-constant ',name ,value-form))))))
