(cl:in-package #:sicl-new-boot-phase-1)

(defun define-environment-functions (client global-environment)
  (setf (clostrum:fdefinition
         client global-environment '(setf fdefinition))
        (lambda (definition name)
          (setf (clostrum:fdefinition client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment 'find-class)
        (lambda (name)
          (clostrum:find-class client global-environment name)))
  (setf (clostrum:fdefinition
         client global-environment '(setf macro-function))
        (lambda (definition name)
          (setf (clostrum:macro-function client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment '(setf compiler-macro-function))
        (lambda (definition name)
          (setf (clostrum:compiler-macro-function client global-environment name)
                definition)))
  (setf (clostrum:fdefinition
         client global-environment '(setf find-class))
        (lambda (class name)
          (setf (clostrum:find-class client global-environment name)
                class)))
  (setf (clostrum:fdefinition client global-environment 'boundp)
        (lambda (name)
          (let ((cell (clo:ensure-variable-cell
                       client global-environment name)))
            (cbae:boundp name cell cbae:*dynamic-environment*))))
  (setf (clostrum:fdefinition client global-environment 'symbol-value)
        (lambda (name)
          (let ((cell (clo:ensure-variable-cell
                       client global-environment name)))
            (cbae:symbol-value name cell cbae:*dynamic-environment*))))
  (setf (clostrum:fdefinition
         client global-environment '(setf symbol-value))
        (lambda (value name)
          (setf (clostrum:symbol-value client global-environment name)
                value)))
  (setf (clostrum:fdefinition
         client global-environment 'define-constant)
        (lambda (name value)
          (clostrum:make-constant client global-environment name value)))
  (setf (clostrum:macro-function client global-environment 'defconstant)
        (lambda (form environment)
          (declare (ignore environment))
          (destructuring-bind (name value-form &optional documentation)
              (rest form)
            (declare (ignore documentation))
            (list 'define-constant (list 'quote name) value-form)))))
