(cl:in-package #:sicl-clos)

(defun check-defgeneric-options-and-methods (options-and-methods)
  (loop for option-or-method in options-and-methods
        do (unless (consp option-or-method)
             (error 'option-or-method-must-be-non-empty-list
                    :expression option-or-method))
           (unless (member (first option-or-method)
                           '(:argument-precedence-order
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
                  :key #'car :test #'eq)
          (remove :method options-and-methods
                  :key #'car :test-not #'eq)))

(defun remove-initial-methods (generic-function)
  (loop for method in (initial-methods generic-function)
        do (remove-method generic-function method))
  (setf (initial-methods generic-function) '()))

;;; FIXME: We handle the :METHOD option by expanding to a DEFMETHOD,
;;; but that is not quite right.  We need to store these methods in a
;;; slot of the generic function so that we can remove them when the
;;; DEFGENERIC form is reevaluated.

(defun defgeneric-expander (name lambda-list options-and-methods)
  (check-defgeneric-options-and-methods options-and-methods)
  (multiple-value-bind (options methods)
      (separate-options-and-methods options-and-methods)
    (let* ((method-combination-option
             (assoc :method-combination options))
           (method-combination-name
             (if (null method-combination-option)
                 'standard
                 (first method-combination-option)))
           (method-combination-arguments
             (if (null method-combination-option)
                 '()
                 (rest method-combination-option)))
           (argument-precedence-order
             (assoc :argument-precedence-order options))
           (generic-function-class-option
             (assoc :generic-function-class options))
           (generic-function-class-name
             (if (null generic-function-class-option)
                 'standard-generic-function
                 (second generic-function-class-option)))
           (method-class-option
             (assoc :method-class options))
           (method-class-name
             (if (null method-class-option)
                 'standard-method
                 (second method-class-option)))
           (documentation-option
             (assoc :documentation options))
           (arg-type (cleavir-code-utilities:lambda-list-type-specifier lambda-list))
           (function-type `(function ,arg-type t))
           (environment-var (gensym)))
      `(progn (eval-when (:compile-toplevel)
                (let ((,environment-var (sicl-genv:global-environment)))
                  (setf (sicl-global-environment:function-lambda-list ',name ,environment-var)
                        ',lambda-list)
                  (setf (sicl-global-environment:function-type ',name ,environment-var)
                        ',function-type)))
              (eval-when (:load-toplevel :execute)
                (let* ((env (sicl-global-environment:global-environment))
                       (fun (ensure-generic-function
                             ',name
                             :lambda-list ',lambda-list
                             ,@(if (null argument-precedence-order)
                                   '()
                                   `(:argument-precedence-order ',(rest argument-precedence-order)))
                             ,@(if (null documentation-option)
                                   '()
                                   `(:documentation ,(second documentation-option)))
                             :method-combination
                             (sicl-clos:find-method-combination
                              (sicl-clos:class-prototype
                               (find-class ',generic-function-class-name))
                              ',method-combination-name
                              ',method-combination-arguments)
                             :generic-function-class
                             (find-class ',generic-function-class-name)
                             :method-class
                             (find-class ',method-class-name)
                             :environment env)))
                  (setf (sicl-global-environment:function-lambda-list ',name env)
                        ',lambda-list)
                  (setf (sicl-global-environment:function-type ',name env)
                        ',function-type)
                  fun))
              ,@(loop for method in methods
                      collect `(defmethod ,name ,@(rest method)))
              (fdefinition ',name)))))
