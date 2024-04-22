(cl:in-package #:sicl-new-boot-phase-1)

;;; DEFCLASS programming.

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  nil)

;;; The function ENSURE-CLASS mentioned here is defined in boot.lisp.
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

;;; This method is meant to provide an expansion that contains a call
;;; to ENSURE-GENERIC-FUNCTION, but we accomplish the action at
;;; macro-expansion time instaed, and provide the expansion NIL.  If
;;; the generic function exists, we do nothing, assuming that a second
;;; invocation of DEFGENERIC is meant to create the same generic
;;; function.  
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
  (declare (ignore documentation-option method-class-name))
  ;; If a generic function named NAME already exists, we assume that
  ;; the second invocation of DEFGENERIC has the same characteristics
  ;; as the first one, so that it is safe to do nothing in that case.
  (unless (clo:fboundp client (sb:environment client) name)
    (let* ((method-combination
             ;; At least SBCL requires a generic function as the first
             ;; argument of FIND-METHOD-COMBINATION, but then this
             ;; argument is ignored, so we can pass any host generic
             ;; function.
             (closer-mop:find-method-combination
              #'print-object
              method-combination-name
              method-combination-arguments))
           (result 
             (make-instance 'standard-generic-function
               :name name
               :lambda-list lambda-list
               :argument-precedence-order argument-precedence-order
               :method-combination method-combination)))
      (setf (clo:fdefinition client (sb:environment client) name)
            result)))
  nil)

;;; PROCLAIM programming.

(defmethod cmd:proclaim ((client client) declaration-specifier environment)
  (when (eq (first declaration-specifier) 'special)
    (let ((name (second declaration-specifier))
          (environment (sb:environment client)))
      (setf (clostrum-sys:variable-status client environment name)
            :special)))
  nil)

;;; DEFMETHOD programming.
;;;
;;; We can't really use the same technique as we did for DEFGENERIC,
;;; i.e., to do the action at macro-expansion time.  The reason is
;;; that the DEFMETHOD form might be evaluated in a non-NULL lexical
;;; environment, so we must provide some expansion of DEFMETHOD that
;;; will then be evaluated in the appropriate lexical environment.

(defmethod cmd:make-method-lambda-wrapper ((client client))
  (lambda (lambda-expression)
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
           (apply ,lambda-expression ,arguments))))))

(defmethod cmd:wrap-in-make-method-lambda
    ((client client)
     lambda-expression
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
  (let* ((environment (sb:environment client))
         (gf (clo:fdefinition client environment function-name)))
    (when (null gf)
      (warn "Creating generic function ~s" function-name)
      (let ((method-combination
              (closer-mop:find-method-combination
               #'print-object 'standard '())))
        (setf gf
              (make-instance 'standard-generic-function
                :name function-name
                :lambda-list lambda-list
                :method-combination method-combination))
        (setf (clo:fdefinition client (sb:environment client) function-name)
              gf)))
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
