(cl:in-package #:sicl-data-and-control-flow)

(symbol-macrolet ((c sicl-client:*client*))
  (let ((e env:*environment*))
    (defun (setf fdefinition) (new-definition function-name)
      (if (env:special-operator c e function-name)
          (error 'attempt-to-set-the-fdefinition-of-a-special-operator
                 :name function-name)
          (progn (setf (env:macro-function c e function-name) nil)
                 (setf (env:fdefinition c e function-name) new-definition)
                 new-definition)))))
