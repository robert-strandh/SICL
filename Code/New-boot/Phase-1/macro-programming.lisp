(cl:in-package #:sicl-new-boot-phase-1)

;;; DEFCLASS programming.

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  nil)

(defun transform-symbol (symbol)
  (let ((name 
          (let* ((*package* (find-package '#:keyword)))
            (format nil "~s" symbol)))
        (package (find-package "SICL-NEW-BOOT-PHASE-1")))
    (intern (string-downcase name) package)))

(defun transform-name (name)
  (etypecase name
    (symbol (transform-symbol name))
    ((cons (eql setf) (cons symbol))
     `(setf ,(transform-symbol (second name))))))

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
  (fmakunbound (transform-name name))
  (let* ((method-combination
           (closer-mop:find-method-combination
            #'print-object
            method-combination-name
            method-combination-arguments))
         (result 
           (ensure-generic-function
            (transform-name name)
            :lambda-list lambda-list
            :argument-precedence-order argument-precedence-order
            :generic-function-class generic-function-class-name
            :method-class method-class-name
            :method-combination method-combination)))
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

(defun ensure-method
    (generic-function method-function lambda-list qualifiers specializers)
  (let ((method
          (make-instance 'standard-method
            :function method-function
            :lambda-list lambda-list
            :specializers specializers
            :qualifiers qualifiers)))
    (add-method generic-function method)))

(defmethod cmd:wrap-in-ensure-method
    ((client client)
     function-name
     lambda-list
     qualifiers
     specializers
     documentation
     method-lambda)
  (let* ((environment (environment client))
         (new-name (transform-name function-name))
         (gf (ensure-generic-function new-name :lambda-list lambda-list)))
    (setf (clostrum:fdefinition client environment function-name) gf)
    `(ensure-method
      ,gf
      ,method-lambda
      ',lambda-list
      ',qualifiers
      ',(loop for specializer in specializers
              collect 
              (if (symbolp specializer)
                  (clo:find-class client environment specializer)
                  (make-instance 'closer-mop:eql-specializer
                    :object (second specializer)))))))
