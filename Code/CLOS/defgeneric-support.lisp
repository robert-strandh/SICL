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

;;; FIXME: We handle the :METHOD option by expanding to a DEFMETHOD,
;;; but that is not quite right.  We need to store these methods in a
;;; slot of the generic function so that we can remove them when the
;;; DEFGENERIC form is reevaluated.

(defun defgeneric-expander (name lambda-list options-and-methods environment)
  (check-defgeneric-options-and-methods options-and-methods)
  (multiple-value-bind (options methods)
      (separate-options-and-methods options-and-methods)
    (let* ((method-combination-option
             (assoc :method-combination options))
           (method-combination-name
             (if (null method-combination-option)
                 'standard
                 (second method-combination-option)))
           (method-combination-arguments
             (if (null method-combination-option)
                 '()
                 (rest (rest method-combination-option))))
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
           (environment-var (gensym))
           (client-var (gensym)))
      `(progn
         (eval-when (:compile-toplevel)
           (let* ((,environment-var
                    (sicl-environment:global-environment ,environment))
                  (client-var (sicl-environment:client ,environment-var)))
             (setf (sicl-environment:function-description
                    ,client-var ,environment-var ',name)
                   (make-instance 'sicl-environment:generic-function-description
                     :lambda-list ',lambda-list
                     :class-name ',generic-function-class-name
                     :method-class-name ',method-class-name
                     :method-combination-info
                     '(,method-combination-name ,@method-combination-arguments)))))
         (eval-when (:load-toplevel :execute)
           (let* ((env (sicl-environment:global-environment)))
             (ensure-generic-function
              ',name
              :lambda-list ',lambda-list
              ,@(if (null argument-precedence-order)
                    '()
                    `(:argument-precedence-order ',(rest argument-precedence-order)))
              ,@(if (null documentation-option)
                    '()
                    `(:documentation ,(second documentation-option)))
              ,@(if (null method-combination-option)
                    '()
                    `(:method-combination
                      (find-method-combination
                       (class-prototype
                        (find-class ',generic-function-class-name))
                       ',method-combination-name
                       ',method-combination-arguments)))
              :generic-function-class ',generic-function-class-name
              :method-class ',method-class-name
              :environment env)))
         ,@(loop for method in methods
                 collect `(defmethod ,name ,@(rest method)))
         (fdefinition ',name)))))
