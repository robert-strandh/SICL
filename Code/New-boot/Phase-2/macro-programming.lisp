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
  'ensure-class)

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
  `(@sicl-clos:ensure-generic-function
    ,name
    :lambda-list lambda-list
    :argument-precedence-order argument-precedence-order
    :generic-function-class generic-function-class-name
    :method-class method-class-name
    :method-combination
    (@clostrophilia:find-method-combination
     ',method-combination-name ',method-combination-arguments)))

;;; PROCLAIM programming.

(defmethod cmd:proclaim ((client client) declaration-specifier environment)
  (when (eq (first declaration-specifier) 'special)
    (let ((name (second declaration-specifier))
          (environment (sb:environment client)))
      (setf (clostrum-sys:variable-status client environment name)
            :special)))
  nil)
