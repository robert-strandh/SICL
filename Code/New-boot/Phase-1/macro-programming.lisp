(cl:in-package #:sicl-new-boot-phase-1)

;;; DEFCLASS programming.

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  nil)

(defun transform-name (name)
  (intern (string-downcase (symbol-name name))
          (find-package "SICL-NEW-BOOT-PHASE-1")))

(defun transform-slot-spec
    (&rest arguments &key readers writers &allow-other-keys)
  (let ((result arguments)
        (new-readers (loop for reader in readers
                           collect  (transform-name reader)))
        (new-writers (loop for writer in writers
                           collect
                           `(setf ,(transform-name (second writer))))))
    (remf result :readers)
    (remf result :writers)
    `(,@result :readers ,new-readers :writers ,new-writers)))

(defmethod cmd:ensure-class
    ((client client)
     name
     superclass-names
     direct-slot-specs
     options
     environment)
  (let ((result
          (closer-mop:ensure-class
           (transform-name name)
           :name name
           :direct-superclasses (mapcar #'transform-name superclass-names)
           :direct-slots (mapcar (lambda (slot-spec)
                                   (apply #'transform-slot-spec slot-spec))
                                 direct-slot-specs)
           :metaclass 'closer-mop:funcallable-standard-class)))
    (setf (clo:find-class client (environment client) name)
          result)))

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
  (let ((result 
          (ensure-generic-function
           (transform-name name)
           :lambda-list lambda-list
           :argument-precedence-order argument-precedence-order
           :generic-function-class generic-function-class-name
           :method-class method-class-name)))
    (setf (clo:fdefinition client (environment client) name)
          result)))

;;; PROCLAIM programming.

(defmethod cmd:proclaim ((client client) declaration-specifier environment)
  (when (eq (first declaration-specifier) 'special)
    (let ((name (second declaration-specifier))
          (environment (environment client)))
      (setf (clostrum-sys:variable-status client environment name)
            :special)))
  nil)

;;; DEFMETHOD programming.

(defmethod cmd:wrap-in-make-method-lambda
    ((client client)
     lambda-expression
     generic-function-name
     environment)
  (let ((arguments (gensym))
        (next-methods (gensym)))
    `(lambda (,arguments ,next-methods)
       (flet ((next-method-p ()
                (not (null ,next-methods)))
              (call-next-method (&rest arguments)
                (when (null ,next-methods)
                  (error "no next method"))
                (funcall (closer-mop:method-function (first ,next-methods))
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
  (closer-mop:ensure-method
   (fdefinition (transform-name function-name))
   method-lambda
   :qualifiers qualifiers
   :lambda-list lambda-list
   :specializers specializers)
  nil)
