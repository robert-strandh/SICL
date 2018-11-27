(cl:in-package #:sicl-boot-phase-4)

(defun define-add-remove-direct-subclass (e4)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e4)
  (load-file "CLOS/add-remove-direct-subclass-support.lisp" e4)
  (load-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e4)
  (load-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e4))

(defun define-add-remove-method (e4)
  (load-file "CLOS/add-remove-method-defgenerics.lisp" e4)
  ;; MAKE-LIST is called when a method is removed and a new
  ;; specializer profile must be computed.
  (import-function-from-host 'make-list e4)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e4)
  (load-file-protected "CLOS/add-remove-method-support.lisp" e4)
  (load-file "CLOS/add-remove-method-defmethods.lisp" e4))

(defun define-add-remove-direct-method (e4)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e4)
  (load-file "CLOS/add-remove-direct-method-defgenerics.lisp" e4)
  (load-file "CLOS/add-remove-direct-method-support.lisp" e4)
  (load-file "CLOS/add-remove-direct-method-defmethods.lisp" e4))

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
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e4)
        (constantly t)))

(defun define-dependent-protocol (e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e4)
        (constantly nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e4)
        (constantly nil)))

(defun define-ensure-class (e4)
  (load-file "CLOS/ensure-class-using-class-support.lisp" e4)
  ;; When we loaded the support code, we defined
  ;; FIND-SUPERCLASS-OR-NIL to call FIND-CLASS, but that's wrong
  ;; during bootstrapping, so we redifine it here.
  (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass-or-nil e4)
        (lambda (name)
          (sicl-genv:find-class name e4)))
  ;; Uncomment this code once we have the class NULL.
  ;; (load-file "CLOS/ensure-class-using-class-defgenerics.lisp" e4)
  ;; (load-file "CLOS/ensure-class-using-class-defmethods.lisp" e4)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e4)
        (lambda (&rest arguments)
          (apply (sicl-genv:fdefinition
                  'sicl-clos::ensure-class-using-class-null e4)
                 nil arguments))))

(defun enable-class-initialization-in-e4 (e3 e4 e5)
  (import-functions-from-host '(sicl-genv:typep) e4)
  (setf (sicl-genv:fdefinition 'typep e4)
        (lambda (object type)
          (sicl-genv:typep object type e4)))
  (define-validate-superclass e4)
  (define-direct-slot-definition-class e3 e4)
  (define-add-remove-direct-subclass e4)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e4 t) nil)
  (define-add-remove-method e4)
  (load-file-protected "CLOS/add-accessor-method.lisp" e4)
  (define-find-or-create-generic-function e4 e5)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass e4)
        (lambda (name)
          (sicl-genv:find-class name e4)))
  (load-file "CLOS/default-superclasses-defgeneric.lisp" e4)
  (load-file "CLOS/default-superclasses-defmethods.lisp" e4)
  (load-file "CLOS/class-initialization-support.lisp" e4)
  (load-file "CLOS/class-initialization-defmethods.lisp" e4)
  (setf (sicl-genv:fdefinition '(setf find-class) e4)
        (lambda (new-class symbol &optional errorp)
          (declare (ignore errorp))
          (setf (sicl-genv:find-class symbol e4) new-class)))
  (define-error-function 'change-class e4)
  (load-file "CLOS/reinitialize-instance-defgenerics.lisp" e4)
  (define-ensure-class e4)
  (import-function-from-host 'sicl-clos:defclass-expander e4)
  (load-file "CLOS/defclass-defmacro.lisp" e4)
  (import-function-from-host '(setf sicl-genv:special-variable) e4)
  (define-error-function 'slot-value e4)
  (define-error-function '(setf slot-value) e4)
  (import-functions-from-host
   '(equal set-exclusive-or sicl-genv:find-class)
   e4)
  ;; FIXME: load files containing the definition instead.
  (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e4)
        (constantly nil))
  (define-dependent-protocol e4)
  (define-reader/writer-method-class e3 e4))
