(cl:in-package #:sicl-boot-phase-3)

(defun define-add-remove-direct-subclass (e3)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e3)
  (load-fasl "CLOS/add-remove-direct-subclass-support.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-subclass-defgenerics.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-subclass-defmethods.fasl" e3))

(defun define-add-remove-method (e3)
  (load-fasl "CLOS/add-remove-method-defgenerics.fasl" e3)
  ;; MAKE-LIST is called when a method is removed and a new
  ;; specializer profile must be computed.
  (import-function-from-host 'make-list e3)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e3)
  (load-fasl "CLOS/add-remove-method-support.fasl" e3)
  (load-fasl "CLOS/add-remove-method-defmethods.fasl" e3))

(defun define-add-remove-direct-method (e3)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e3)
  (load-fasl "CLOS/add-remove-direct-method-defgenerics.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-method-support.fasl" e3)
  (load-fasl "CLOS/add-remove-direct-method-defmethods.fasl" e3))

(defun define-reader/writer-method-class (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class e3)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e2)))
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class e3)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e2))))

(defun define-direct-slot-definition-class (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e3)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e2))))

(defun define-find-or-create-generic-function (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e3)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e4))))

(defun define-validate-superclass (e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e3)
        (constantly t)))

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
    (define-reader/writer-method-class e2 e3)
    (import-functions-from-host '(adjoin set-exclusive-or) e3)
    (define-add-remove-direct-method e3)
    (define-dependent-protocol e3)
    (define-add-remove-method e3)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e3 nil) nil)
    (load-fasl "CLOS/add-accessor-method.fasl" e3)
    (define-find-or-create-generic-function e3 e4)
    (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e3)
          (lambda (class) (declare (ignore class)) '()))
    (import-function-from-host 'sicl-genv:typep e3)
    (load-fasl "CLOS/class-initialization-support.fasl" e3)
    (load-fasl "CLOS/class-initialization-defmethods.fasl" e3)
    (define-ensure-class e2 e3)))
