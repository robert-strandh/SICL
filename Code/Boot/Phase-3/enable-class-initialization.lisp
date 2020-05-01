(cl:in-package #:sicl-boot-phase-3)

(defun define-add-remove-direct-subclass (e3)
  (load-fasl "CLOS/add-remove-direct-subclass-support.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-subclass-defgenerics.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-subclass-defmethods.fasl" e3))

(defun define-add-remove-method (e3)
  (load-fasl "CLOS/add-remove-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/add-remove-method-support.fasl" e3)
  (load-fasl "CLOS/add-remove-method-defmethods.fasl" e3))

(defun define-add-remove-direct-method (e3)
  (load-fasl "CLOS/add-remove-direct-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-method-support.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-method-defmethods.fasl" e3))

(defun define-reader/writer-method-class (e2 e3)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::reader-method-class-default
        sicl-clos::writer-method-class-default)
       e2 e3)
    (load-fasl "CLOS/reader-writer-method-class-support.fasl" e2))
  (load-fasl "CLOS/reader-writer-method-class-defgenerics.fasl" e3)
  (load-fasl "CLOS/reader-writer-method-class-defmethods.fasl" e3))

(defun define-direct-slot-definition-class (e2 e3)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::direct-slot-definition-class-default)
       e2 e3)
    (load-fasl "CLOS/direct-slot-definition-class-support.fasl" e2))
  (load-fasl "CLOS/direct-slot-definition-class-defgeneric.fasl" e3)
  (load-fasl "CLOS/direct-slot-definition-class-defmethods.fasl" e3))

(defun define-find-or-create-generic-function (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e3)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e4))))

(defun define-validate-superclass (e3)
  (load-fasl "CLOS/validate-superclass-defgenerics.fasl" e3)
  (load-fasl "CLOS/validate-superclass-defmethods.fasl" e3))

(defun define-dependent-protocol (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e3)
        (constantly nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e3)
        (constantly nil)))

(defun define-ensure-class (e2 e3)
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
                                     collect (sicl-genv:find-class name e3)))
                 (remaining-keys (copy-list keys)))
            (loop while (remf remaining-keys :metaclass))
            (loop while (remf remaining-keys :direct-superclasses))
            (setf (sicl-genv:find-class name e3)
                  (apply #'make-instance metaclass-class
                         :direct-default-initargs direct-default-initargs
                         :direct-slots direct-slots
                         :direct-superclasses superclasses
                         :name name
                         remaining-keys))))))

(defun enable-class-initialization (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (define-validate-superclass e3)
    (define-direct-slot-definition-class e2 e3)
    (define-add-remove-direct-subclass e3)
    (load-fasl "Environment/find-class-defun.fasl" e2)
    (load-fasl "Environment/find-class-defun.fasl" e3)
    (define-reader/writer-method-class e2 e3)
    (define-add-remove-direct-method e3)
    (define-dependent-protocol e3)
    (define-add-remove-method e3)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e3 nil) nil)
    (load-fasl "CLOS/add-accessor-method.fasl" e3)
    (define-find-or-create-generic-function e3 e4)
    (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e3)
          (lambda (class) (declare (ignore class)) '()))
    (load-fasl "CLOS/class-initialization-support.fasl" e3)
    (load-fasl "CLOS/class-initialization-defmethods.fasl" e3)
    (define-ensure-class e2 e3)))
