(cl:in-package #:sicl-new-boot-phase-3)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'function)) (environment environment))
  (typep object 'function))

;;; We only check for the type CLASS for reasons of catching errors,
;;; but during bootstrapping, we completely control the arguments, so
;;; we can simply return true here.
(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  t)

;;; This function defines functions ADD-READER-METHOD and
;;; ADD-WRITER-METHOD in E3 that look up their generic functions in
;;; E4.  They do not call ADD-METHOD, because ADD-METHOD is defined to
;;; add methods to generic functions in E3.  The other defect these
;;; functions have is that they do not set the specializer profile on
;;; the generic function, so we have to do that manually in phase 4.
(defun define-add-accessor-method (e2 e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::add-reader-method e3)
        (lambda (class function-name slot-definition)
          (let* ((lambda-list '(object))
                 (generic-function (sicl-genv:fdefinition function-name e4))
                 (specializers (list class))
                 (slot-name (slot-value slot-definition 'sicl-clos::%name))
                 (method-function
                   (compile nil `(lambda (arguments next-methods)
                                   (declare (ignore next-methods))
                                   (slot-value (car arguments) ',slot-name))))
                 (method-class (sicl-genv:find-class
                                'sicl-clos:standard-reader-method e2))
                 (method (make-instance method-class
                           :lambda-list lambda-list
                           :qualifiers '()
                           :specializers specializers
                           :function method-function
                           :slot-definition slot-definition)))
            (push method (slot-value generic-function 'sicl-clos::%methods)))))
  (setf (sicl-genv:fdefinition 'sicl-clos::add-writer-method e3)
        (lambda (class function-name slot-definition)
          (let* ((lambda-list '(object))
                 (generic-function (sicl-genv:fdefinition function-name e4))
                 (specializers (list (sicl-genv:find-class 't e3) class))
                 (slot-name (slot-value slot-definition 'sicl-clos::%name))
                 (method-function
                   (compile nil `(lambda (arguments next-methods)
                                   (declare (ignore next-methods))
                                   (setf (slot-value (cadr arguments) ',slot-name)
                                         (car arguments)))))
                 (method-class (sicl-genv:find-class
                                'sicl-clos:standard-writer-method e2))
                 (method (make-instance method-class
                           :lambda-list lambda-list
                           :qualifiers '()
                           :specializers specializers
                           :function method-function
                           :slot-definition slot-definition)))
            (push method (slot-value generic-function 'sicl-clos::%methods))))))

(defun define-ensure-class (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e3)
          (lambda (name
                   &rest keys
                   &key
                     direct-default-initargs
                     direct-slots
                     direct-superclasses
                     (metaclass nil metaclass-p)
                   &allow-other-keys)
            (let* ((metaclass-name (if metaclass-p metaclass 'standard-class))
                   (metaclass-class (sicl-genv:find-class metaclass-name e2))
                   (superclass-names
                     (if (null direct-superclasses)
                         (cond ((eq metaclass-name 'standard-class)
                                '(standard-object))
                               ((eq metaclass-name
                                    'sicl-clos:funcallable-standard-class)
                                '(funcallable-standard-object))
                               (t '()))
                         direct-superclasses))
                   (superclasses (loop for name in superclass-names
                                       collect (sicl-genv:find-class name e2)))
                   (remaining-keys (copy-list keys)))
              (loop while (remf remaining-keys :metaclass))
              (loop while (remf remaining-keys :direct-superclasses))
              (setf (sicl-genv:find-class name e3)
                    (apply (sicl-genv:fdefinition 'make-instance e3)
                           metaclass-class
                           :direct-default-initargs direct-default-initargs
                           :direct-slots direct-slots
                           :direct-superclasses superclasses
                           :name name
                           remaining-keys)))))))

(defun create-mop-classes (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4))
      boot
    (import-functions-from-host '(sicl-genv:typep) e3)
    (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e3)
          (lambda (class direct-superclass)
            (declare (ignore class direct-superclass))
            t))
    (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e3)
          (lambda (&rest arguments)
            (declare (ignore arguments))
            (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e2)))
    (import-functions-from-host '(remove) e3)
    (load-file "CLOS/add-remove-direct-subclass-support.lisp" e3)
    (load-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e3)
    (load-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e3)
    (define-add-accessor-method e2 e3 e4)
    (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e3)
          (lambda (class) (declare (ignore class)) '()))
    (load-file "CLOS/class-initialization-support.lisp" e3)
    (load-file "CLOS/class-initialization-defmethods.lisp" e3)
    (sicl-minimal-extrinsic-environment:import-function-from-host
     'sicl-clos:defclass-expander e3)
    (define-ensure-class boot)
    (load-file "CLOS/defclass-defmacro.lisp" e3)
    (import-function-from-host '(setf sicl-genv:special-variable) e3)
    (satiate-all-functions e1 e2 e3)
    (load-file "CLOS/t-defclass.lisp" e3)
    (load-file "CLOS/function-defclass.lisp" e3)
    (load-file "CLOS/standard-object-defclass.lisp" e3)
    (load-file "CLOS/metaobject-defclass.lisp" e3)
    (load-file "CLOS/method-defclass.lisp" e3)
    (load-file "CLOS/standard-method-defclass.lisp" e3)
    (load-file "CLOS/standard-accessor-method-defclass.lisp" e3)
    (load-file "CLOS/standard-reader-method-defclass.lisp" e3)
    (load-file "CLOS/standard-writer-method-defclass.lisp" e3)
    (load-file "CLOS/slot-definition-defclass.lisp" e3)
    (load-file "CLOS/standard-slot-definition-defclass.lisp" e3)
    (load-file "CLOS/direct-slot-definition-defclass.lisp" e3)
    (load-file "CLOS/effective-slot-definition-defclass.lisp" e3)
    (load-file "CLOS/standard-direct-slot-definition-defclass.lisp" e3)
    (load-file "CLOS/standard-effective-slot-definition-defclass.lisp" e3)
    (load-file "CLOS/method-combination-defclass.lisp" e3)
    (load-file "CLOS/specializer-defclass.lisp" e3)
    (load-file "CLOS/eql-specializer-defclass.lisp" e3)
    (load-file "CLOS/class-unique-number-defparameter.lisp" e3)
    (load-file "CLOS/class-defclass.lisp" e3)
    (load-file "CLOS/forward-referenced-class-defclass.lisp" e3)
    (load-file "CLOS/real-class-defclass.lisp" e3)
    (load-file "CLOS/regular-class-defclass.lisp" e3)
    (load-file "CLOS/standard-class-defclass.lisp" e3)
    (load-file "CLOS/funcallable-standard-class-defclass.lisp" e3)
    (load-file "CLOS/built-in-class-defclass.lisp" e3)
    (load-file "CLOS/funcallable-standard-object-defclass.lisp" e3)
    (load-file "CLOS/generic-function-defclass.lisp" e3)
    (load-file "CLOS/standard-generic-function-defclass.lisp" e3)))
