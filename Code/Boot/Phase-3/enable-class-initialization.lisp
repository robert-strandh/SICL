(cl:in-package #:sicl-boot-phase-3)

(defun define-add-remove-direct-subclass (e3)
  (load-source "CLOS/add-remove-direct-subclass-support.lisp" e3)
  (load-source "CLOS/add-remove-direct-subclass-defgenerics.lisp" e3)
  (load-source "CLOS/add-remove-direct-subclass-defmethods.lisp" e3))

(defun define-add-remove-method (e3)
  (load-source "CLOS/add-remove-method-defgenerics.lisp" e3)
  (load-source "CLOS/add-remove-method-support.lisp" e3)
  (load-source "CLOS/add-remove-method-defmethods.lisp" e3))

(defun define-add-remove-direct-method (e3)
  (load-source "CLOS/add-remove-direct-method-defgenerics.lisp" e3)
  (load-source "CLOS/add-remove-direct-method-support.lisp" e3)
  (load-source "CLOS/add-remove-direct-method-defmethods.lisp" e3))

(defun define-reader/writer-method-class (e2 e3)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::reader-method-class-default
        sicl-clos::writer-method-class-default)
       e3)
    (load-source "CLOS/reader-writer-method-class-support.lisp" e2))
  (load-source "CLOS/reader-writer-method-class-defgenerics.lisp" e3)
  (load-source "CLOS/reader-writer-method-class-defmethods.lisp" e3))

(defun define-direct-slot-definition-class (e2 e3)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::direct-slot-definition-class-default)
       e3)
    (load-source "CLOS/direct-slot-definition-class-support.lisp" e2))
  (load-source "CLOS/direct-slot-definition-class-defgeneric.lisp" e3)
  (load-source "CLOS/direct-slot-definition-class-defmethods.lisp" e3))

(defun define-find-or-create-generic-function (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e3)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e4))))

(defun define-validate-superclass (e3)
  (load-source "CLOS/validate-superclass-defgenerics.lisp" e3)
  (load-source "CLOS/validate-superclass-defmethods.lisp" e3))

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
    (load-source "Environment/find-class-defun.lisp" e2)
    (load-source "Environment/find-class-defun.lisp" e3)
    (define-reader/writer-method-class e2 e3)
    (define-add-remove-direct-method e3)
    (define-dependent-protocol e3)
    (define-add-remove-method e3)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e3 nil) nil)
    (load-source "CLOS/add-accessor-method.lisp" e3)
    (define-find-or-create-generic-function e3 e4)
    (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e3)
          (lambda (class) (declare (ignore class)) '()))
    (load-source "CLOS/class-finalization-support.lisp" e3)
    (load-source "CLOS/class-initialization-support.lisp" e3)
    (load-source "CLOS/class-initialization-defmethods.lisp" e3)
    (define-ensure-class e2 e3)))
