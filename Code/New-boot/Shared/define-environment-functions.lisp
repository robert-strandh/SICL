(cl:in-package #:sicl-new-boot)

(defun define-environment-functions (client global-environment)
  (setf (clo:fdefinition
         client global-environment 'fboundp)
        (lambda (name)
          (clo:fboundp client global-environment name)))
  (setf (clo:fdefinition
         client global-environment 'fdefinition)
        (lambda (name)
          (clo:fdefinition client global-environment name)))
  (setf (clo:fdefinition
         client global-environment '(setf fdefinition))
        (lambda (definition name)
          (setf (clo:fdefinition client global-environment name)
                definition)))
  (setf (clo:fdefinition
         client global-environment 'find-class)
        (lambda (name)
          (clo:find-class client global-environment name)))
  (setf (clo:fdefinition
         client global-environment 'macro-function)
        (lambda (name)
          (clo:macro-function client global-environment name)))
  (setf (clo:fdefinition
         client global-environment '(setf macro-function))
        (lambda (definition name)
          (setf (clo:macro-function client global-environment name)
                definition)))
  (setf (clo:fdefinition
         client global-environment '(setf compiler-macro-function))
        (lambda (definition name)
          (setf (clo:compiler-macro-function client global-environment name)
                definition)))
  (setf (clo:fdefinition
         client global-environment '(setf find-class))
        (lambda (class name)
          (setf (clo:find-class client global-environment name)
                class)))
  (setf (clo:fdefinition client global-environment 'boundp)
        (lambda (name)
          (let ((cell (clo:ensure-variable-cell
                       client global-environment name)))
            (cbae:boundp name cell cbae:*dynamic-environment*))))
  (setf (clo:fdefinition client global-environment 'symbol-value)
        (lambda (name)
          (let ((cell (clo:ensure-variable-cell
                       client global-environment name)))
            (cbae:symbol-value name cell cbae:*dynamic-environment*))))
  (setf (clo:fdefinition
         client global-environment '(setf symbol-value))
        (lambda (value name)
          (let ((cell (clo:ensure-variable-cell
                       client global-environment name)))
            (setf (cbae:symbol-value name cell cbae:*dynamic-environment*)
                  value))))
  (setf (clo:fdefinition
         client global-environment 'define-constant)
        (lambda (name value)
          (clo:make-constant client global-environment name value)))
  (setf (clo:macro-function client global-environment 'defconstant)
        (lambda (form environment)
          (declare (ignore environment))
          (destructuring-bind (name value-form &optional documentation)
              (rest form)
            (declare (ignore documentation))
            (list 'define-constant (list 'quote name) value-form)))))
