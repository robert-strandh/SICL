(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

;;; DEFCLASS programming.

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  nil)

(defmethod cmd:ensure-class-name ((client client))
  @sicl-clos:ensure-class)

;;; DEFGENERIC programming.

(defmethod cmd:defgeneric-compile-time-action
    ((client client)
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment)
  nil)

(defmethod cmd:ensure-generic-function
    ((client client)
     name
     lambda-list
     argument-precedence-order
     generic-function-class-name
     method-class-name
     method-combination-name
     method-combination-arguments
     documentation-option
     environment)
  (declare (ignore documentation-option environment))
  (let ((find-method-combination-symbol
          @clostrophilia:find-method-combination))
    `(ensure-generic-function
      ',name
      :lambda-list ',lambda-list
      ,@(if (null argument-precedence-order)
            '()
            `(:argument-precedence-order ',(cdr argument-precedence-order)))
      :generic-function-class ',generic-function-class-name
      :method-class ',method-class-name
      :method-combination
      (funcall #',find-method-combination-symbol
       nil ',method-combination-name ',method-combination-arguments))))

;;; PROCLAIM programming.

(defmethod cmd:proclaim ((client client) declaration-specifier environment)
  (when (eq (first declaration-specifier) 'special)
    (let ((name (second declaration-specifier))
          (environment (sb:environment client)))
      (setf (clostrum-sys:variable-status client environment name)
            :special)))
  nil)

;;; DEFMETHOD programming.

(defmethod cmd:make-method-lambda-wrapper ((client client))
  (lambda (lambda-expression)
    (let ((arguments (gensym))
          (next-methods (gensym))
          (method-function-name @sicl-clos:^method-function))
      `(lambda (,arguments ,next-methods)
         (flet ((next-method-p ()
                  (not (null ,next-methods)))
                (call-next-method (&rest arguments)
                  (when (null ,next-methods)
                    (error "no next method"))
                  (funcall (,method-function-name (first ,next-methods))
                           (or arguments ,arguments)
                           (rest ,next-methods))))
           (declare (ignorable #'next-method-p #'call-next-method))
           (apply ,lambda-expression ,arguments))))))

(defmethod cmd:wrap-in-make-method-lambda
    ((client client)
     lambda-expression
     environment)
  (let ((arguments (gensym))
        (next-methods (gensym))
        (method-function-name @sicl-clos:^method-function))
    `(lambda (,arguments ,next-methods)
       (flet ((next-method-p ()
                (not (null ,next-methods)))
              (call-next-method (&rest arguments)
                (when (null ,next-methods)
                  (error "no next method"))
                (funcall (,method-function-name (first ,next-methods))
                         (or arguments ,arguments)
                         (rest ,next-methods))))
         (declare (ignorable #'next-method-p #'call-next-method))
         (apply ,lambda-expression ,arguments)))))

(defmethod cmd:wrap-in-ensure-method
    ((client client)
     function-name
     lambda-list
     qualifiers
     specializers
     documentation
     method-lambda)
  (let ((symbol @sicl-clos:ensure-method))
    `(,symbol
      ',function-name
      :unspecialized-lambda-list ',lambda-list
      :qualifiers ',qualifiers
      :specializer-designators ',specializers
      :documentation ',documentation
      :function ,method-lambda)))
