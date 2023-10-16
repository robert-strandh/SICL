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
  (let ((package (find-package "SICL-NEW-BOOT-PHASE-1")))
    (etypecase name
      (symbol
       (intern (string-downcase (symbol-name name))
               package))
      ((cons (eql setf) (cons symbol))
       `(setf ,(intern (string-downcase (symbol-name (second name)))
                       package))))))

(defun transform-slot-spec (slot-spec)
  (let ((result (copy-list slot-spec)))
    (let ((readers (member :readers result)))
      (unless (null readers)
        (setf (second readers)
              (loop for reader in (second readers)
                    collect  (transform-name reader)))))
    (let ((writers (member :writers result)))
      (unless (null writers)
        (setf (second writers)
              (loop for writer in (second writers)
                    collect  (transform-name writer)))))
    (let ((initfunction (member :initfunction result)))
      (unless (null initfunction)
        (setf (second initfunction)
              (eval (second initfunction)))))
    result))

(defmethod cmd:ensure-class
    ((client client)
     name
     superclass-names
     direct-slot-specs
     options
     environment)
  (setf (find-class (transform-name name)) nil)
  (let ((result
          (closer-mop:ensure-class
           (transform-name name)
           :name name
           :direct-superclasses (mapcar #'transform-name superclass-names)
           :direct-slots (mapcar (lambda (slot-spec)
                                   (transform-slot-spec slot-spec))
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
  (fmakunbound (transform-name name))
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
  (let ((environment (environment client)))
    (let ((method
            (make-instance 'standard-method
              :function (eval method-lambda)
              :qualifiers qualifiers
              :lambda-list lambda-list
              :specializers
              (loop for specializer in specializers
                    collect
                    (clo:find-class client environment specializer)))))
      (add-method (fdefinition (transform-name function-name)) method)))
  nil)
