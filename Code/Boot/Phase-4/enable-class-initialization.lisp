(cl:in-package #:sicl-boot-phase-4)

(defun define-add-remove-direct-subclass (e4)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e4)
  (load-fasl "CLOS/add-remove-direct-subclass-support.fasl" e4)
  (load-fasl "CLOS/add-remove-direct-subclass-defgenerics.fasl" e4)
  (load-fasl "CLOS/add-remove-direct-subclass-defmethods.fasl" e4))

(defun define-add-remove-method (e4)
  (load-fasl "CLOS/add-remove-method-defgenerics.fasl" e4)
  ;; MAKE-LIST is called when a method is removed and a new
  ;; specializer profile must be computed.
  (import-function-from-host 'make-list e4)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e4)
  (load-fasl "CLOS/add-remove-method-support.fasl" e4)
  (load-fasl "CLOS/add-remove-method-defmethods.fasl" e4))

(defun define-add-remove-direct-method (e4)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e4)
  (load-fasl "CLOS/add-remove-direct-method-defgenerics.fasl" e4)
  (load-fasl "CLOS/add-remove-direct-method-support.fasl" e4)
  (load-fasl "CLOS/add-remove-direct-method-defmethods.fasl" e4))

(defun define-reader/writer-method-class (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class e4)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e3)))
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class e4)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e3))))

(defun define-direct-slot-definition-class (e3 e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e4)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e3))))

(defun define-find-or-create-generic-function (e4 e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e4)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e5))))

(defun define-validate-superclass (e4)
  (load-fasl "CLOS/validate-superclass-defgenerics.fasl" e4)
  (load-fasl "CLOS/validate-superclass-defmethods.fasl" e4))

(defun define-dependent-protocol (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e4)
        (constantly nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e4)
        (constantly nil)))

(defun define-ensure-class (e4)
  (load-fasl "CLOS/ensure-class-using-class-support.fasl" e4)
  (load-fasl "CLOS/ensure-class-using-class-defgenerics.fasl" e4)
  (load-fasl "CLOS/ensure-class-using-class-defmethods.fasl" e4)
  (import-function-from-host '(setf sicl-genv:type-expander) e4)
  (import-function-from-host '(setf sicl-genv:find-class) e4)
  (load-fasl "Environment/find-class-defun.fasl" e4)
  (load-fasl "Environment/standard-environment-functions.fasl" e4)
  (load-fasl "CLOS/ensure-class.fasl" e4))

(defun enable-class-initialization (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (import-functions-from-host '(sicl-genv:typep) e4)
    (setf (sicl-genv:fdefinition 'typep e4)
          (lambda (object type)
            (sicl-genv:typep object type e4)))
    (define-validate-superclass e4)
    (define-direct-slot-definition-class e3 e4)
    (define-add-remove-direct-subclass e4)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e4 t) nil)
    (define-add-remove-method e4)
    (load-fasl "CLOS/add-accessor-method.fasl" e4)
    (define-find-or-create-generic-function e4 e5)
    (load-fasl "CLOS/default-superclasses-defgeneric.fasl" e4)
    (load-fasl "CLOS/default-superclasses-defmethods.fasl" e4)
    (load-fasl "CLOS/class-initialization-support.fasl" e4)
    (load-fasl "CLOS/class-initialization-defmethods.fasl" e4)
    (load-fasl "CLOS/reinitialize-instance-defgenerics.fasl" e4)
    (define-ensure-class e4)
    (import-function-from-host '(setf sicl-genv:special-variable) e4)
    (import-functions-from-host
     '(equal set-exclusive-or sicl-genv:find-class)
     e4)
    ;; FIXME: load files containing the definition instead.
    (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e4)
          (constantly nil))
    (define-dependent-protocol e4)
    (define-reader/writer-method-class e3 e4)))
