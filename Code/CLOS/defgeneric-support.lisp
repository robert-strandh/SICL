(cl:in-package #:sicl-clos)

(defun check-defgeneric-options-and-methods (options-and-methods)
  (loop for option-or-method in options-and-methods
        do (unless (consp option-or-method)
             (error 'option-or-method-must-be-non-empty-list
                    :expression option-or-method))
           (unless (member option-or-method '(:argument-precedence-order
                                              declare
                                              :documentation
                                              :method-combination
                                              :generic-function-class
                                              :method-class
                                              :method))
             ;; FIXME: Define signal type.
             (error 'unknown-defgeneric-option
                    :option option-or-method))))

(defun separate-options-and-methods (options-and-methods)
  (values (remove :method options-and-methods
                  :key #'car :test-not #'eq)
          (remove :method options-and-methods
                  :key #'car :test #'eq)))
