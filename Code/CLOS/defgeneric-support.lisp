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

(defun remove-initial-methods (generic-function)
  (loop for method in (initial-methods generic-function)
        do (remove-method generic-function method))
  (setf (initial-methods generic-function) '()))

;;; FIXME: Add options and methods
(defun defgeneric-expander (env name lambda-list options-and-methods)
  (check-defgeneric-options-and-methods options-and-methods)
  (let* ((arg-type (cleavir-code-utilities:lambda-list-type-specifier lambda-list))
         (function-type `(function ,arg-type t)))
    `(progn (eval-when (:compile-toplevel)
              (setf (sicl-global-environment:function-lambda-list ',name ,env)
                    ',lambda-list)
              (setf (sicl-global-environment:function-type ',name ,env)
                    ',function-type))
            (eval-when (:load-toplevel :execute)
              (let* ((env (sicl-global-environment:global-environment))
                     (fun (ensure-generic-function
                           ',name
                           :lambda-list ',lambda-list
                           :environment env)))
                (setf (sicl-global-environment:function-lambda-list ',name env)
                      ',lambda-list)
                (setf (sicl-global-environment:function-type ',name env)
                      ',function-type)
                fun)))))
