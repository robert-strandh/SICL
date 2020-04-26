(cl:in-package #:sicl-boot-phase-5)

(defun define-add-remove-direct-subclass (e5)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e5)
  (load-fasl "CLOS/add-remove-direct-subclass-support.fasl" e5)
  (load-fasl "CLOS/add-remove-direct-subclass-defgenerics.fasl" e5)
  (load-fasl "CLOS/add-remove-direct-subclass-defmethods.fasl" e5))

(defun define-add-remove-method (e5)
  (load-fasl "CLOS/add-remove-method-defgenerics.fasl" e5)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e5)
  (load-fasl "CLOS/add-remove-method-support.fasl" e5)
  (load-fasl "CLOS/add-remove-method-defmethods.fasl" e5))

(defun define-add-remove-direct-method (e5)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e5)
  (load-fasl "CLOS/add-remove-direct-method-defgenerics.fasl" e5)
  (load-fasl "CLOS/add-remove-direct-method-support.fasl" e5)
  (load-fasl "CLOS/add-remove-direct-method-defmethods.fasl" e5))

(defun define-reader/writer-method-class (e4 e5)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::reader-method-class-default
        sicl-clos::writer-method-class-default )
       e4 e5)
    (load-fasl "CLOS/reader-writer-method-class-support.fasl" e4))
  (load-fasl "CLOS/reader-writer-method-class-defgenerics.fasl" e5)
  (load-fasl "CLOS/reader-writer-method-class-defmethods.fasl" e5))

(defun define-direct-slot-definition-class (e4 e5)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::direct-slot-definition-class-default)
       e4 e5)
    (load-fasl "CLOS/direct-slot-definition-class-support.fasl" e4))
  (load-fasl "CLOS/direct-slot-definition-class-defgeneric.fasl" e5)
  (load-fasl "CLOS/direct-slot-definition-class-defmethods.fasl" e5))

(defun define-find-or-create-generic-function (e5 e6)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e5)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e6))))

(defun define-validate-superclass (e5)
  (load-fasl "CLOS/validate-superclass-defgenerics.fasl" e5)
  (load-fasl "CLOS/validate-superclass-defmethods.fasl" e5))

(defun define-dependent-protocol (e5)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e5)
        (constantly nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e5)
        (constantly nil)))

(defun define-ensure-class (e5)
  (load-fasl "CLOS/ensure-class-using-class-support.fasl" e5)
  ;; When we loaded the support code, we defined
  ;; FIND-SUPERCLASS-OR-NIL to call FIND-CLASS, but that's wrong
  ;; during bootstrapping, so we redifine it here.
  (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass-or-nil e5)
        (lambda (name)
          (sicl-genv:find-class name e5)))
  (load-fasl "CLOS/ensure-class-using-class-defgenerics.fasl" e5)
  (load-fasl "CLOS/ensure-class-using-class-defmethods.fasl" e5)
  (import-function-from-host '(setf sicl-genv:type-expander) e5)
  (import-function-from-host '(setf sicl-genv:find-class) e5)
  (load-fasl "Environment/find-class-defun.fasl" e5)
  (load-fasl "Environment/standard-environment-functions.fasl" e5)
  (load-fasl "CLOS/ensure-class.fasl" e5))

(defun enable-class-initialization (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (import-functions-from-host '(sicl-genv:typep) e5)
    (setf (sicl-genv:fdefinition 'typep e5)
          (lambda (object type)
            (sicl-genv:typep object type e5)))
    (define-validate-superclass e5)
    (define-direct-slot-definition-class e4 e5)
    (define-add-remove-direct-subclass e5)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e5 t) nil)
    (define-add-remove-method e5)
    (load-fasl "CLOS/add-accessor-method.fasl" e5)
    (define-find-or-create-generic-function e5 e6)
    (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass e5)
          (lambda (name)
            (sicl-genv:find-class name e5)))
    (load-fasl "CLOS/default-superclasses-defgeneric.fasl" e5)
    (load-fasl "CLOS/default-superclasses-defmethods.fasl" e5)
    (load-fasl "CLOS/class-initialization-support.fasl" e5)
    (load-fasl "CLOS/class-initialization-defmethods.fasl" e5)
    (load-fasl "CLOS/reinitialize-instance-defgenerics.fasl" e5)
    (define-ensure-class e5)
    (import-function-from-host '(setf sicl-genv:special-variable) e5)
    (import-functions-from-host
     '(equal set-exclusive-or sicl-genv:find-class)
     e5)
    ;; FIXME: load files containing the definition instead.
    (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e5)
          (constantly nil))
    (define-dependent-protocol e5)
    (define-reader/writer-method-class e4 e5)))
