(cl:in-package #:sicl-boot-phase-6)

(defun define-add-remove-direct-subclass (e6)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e6)
  (load-fasl "CLOS/add-remove-direct-subclass-support.fasl" e6)
  (load-fasl "CLOS/add-remove-direct-subclass-defgenerics.fasl" e6)
  (load-fasl "CLOS/add-remove-direct-subclass-defmethods.fasl" e6))

(defun define-add-remove-method (e6)
  (load-fasl "CLOS/add-remove-method-defgenerics.fasl" e6)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e6)
  (load-fasl "CLOS/add-remove-method-support.fasl" e6)
  (load-fasl "CLOS/add-remove-method-defmethods.fasl" e6))

(defun define-add-remove-direct-method (e6)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e6)
  (load-fasl "CLOS/add-remove-direct-method-defgenerics.fasl" e6)
  (load-fasl "CLOS/add-remove-direct-method-support.fasl" e6)
  (load-fasl "CLOS/add-remove-direct-method-defmethods.fasl" e6))

(defun define-reader/writer-method-class (e5 e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class e6)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e5)))
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class e6)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e5))))

(defun define-direct-slot-definition-class (e5 e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e6)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e5))))

(defun define-find-or-create-generic-function (e6 e7)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e6)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e7))))

(defun define-validate-superclass (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e6)
        (constantly t)))

(defun define-dependent-protocol (e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e6)
        (constantly nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e6)
        (constantly nil)))

(defun define-ensure-class (e6)
  (load-fasl "CLOS/ensure-class-using-class-support.fasl" e6)
  ;; When we loaded the support code, we defined
  ;; FIND-SUPERCLASS-OR-NIL to call FIND-CLASS, but that's wrong
  ;; during bootstrapping, so we redifine it here.
  (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass-or-nil e6)
        (lambda (name)
          (sicl-genv:find-class name e6)))
  ;; Uncomment this code once we have the class NULL.
  ;; (load-fasl "CLOS/ensure-class-using-class-defgenerics.fasl" e6)
  ;; (load-fasl "CLOS/ensure-class-using-class-defmethods.fasl" e6)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e6)
        (lambda (&rest arguments)
          (apply (sicl-genv:fdefinition
                  'sicl-clos::ensure-class-using-class-null e6)
                 nil arguments))))

(defun enable-class-initialization (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6)
                   (e7 sicl-boot:e7))
      boot
    (import-functions-from-host '(sicl-genv:typep) e6)
    (setf (sicl-genv:fdefinition 'typep e6)
          (lambda (object type)
            (sicl-genv:typep object type e6)))
    (define-validate-superclass e6)
    (define-direct-slot-definition-class e5 e6)
    (define-add-remove-direct-subclass e6)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e6 t) nil)
    (define-add-remove-method e6)
    (load-fasl "CLOS/add-accessor-method.fasl" e6)
    (define-find-or-create-generic-function e6 e7)
    (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass e6)
          (lambda (name)
            (sicl-genv:find-class name e6)))
    (load-fasl "CLOS/default-superclasses-defgeneric.fasl" e6)
    (load-fasl "CLOS/default-superclasses-defmethods.fasl" e6)
    (load-fasl "CLOS/class-initialization-support.fasl" e6)
    (load-fasl "CLOS/class-initialization-defmethods.fasl" e6)
    (setf (sicl-genv:fdefinition '(setf find-class) e6)
          (lambda (new-class symbol &optional errorp)
            (declare (ignore errorp))
            (setf (sicl-genv:find-class symbol e6) new-class)))
    (load-fasl "CLOS/reinitialize-instance-defgenerics.fasl" e6)
    (define-ensure-class e6)
    (import-function-from-host '(setf sicl-genv:special-variable) e6)
    (import-functions-from-host
     '(equal set-exclusive-or sicl-genv:find-class)
     e6)
    ;; FIXME: load files containing the definition instead.
    (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e6)
          (constantly nil))
    (define-dependent-protocol e6)
    (define-reader/writer-method-class e5 e6)))
