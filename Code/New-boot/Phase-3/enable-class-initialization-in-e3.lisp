(cl:in-package #:sicl-new-boot-phase-3)

(defun define-add-remove-direct-subclass (e3)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e3)
  (load-file "CLOS/add-remove-direct-subclass-support.lisp" e3)
  (load-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e3)
  (load-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e3))

(defun define-add-remove-method (e3)
  (load-file "CLOS/add-remove-method-defgenerics.lisp" e3)
  ;; MAKE-LIST is called when a method is removed and a new
  ;; specializer profile must be computed.
  (import-function-from-host 'make-list e3)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e3)
  (load-file-protected "CLOS/add-remove-method-support.lisp" e3)
  (load-file "CLOS/add-remove-method-defmethods.lisp" e3))

(defun define-add-remove-direct-method (e3)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e3)
  (load-file "CLOS/add-remove-direct-method-defgenerics.lisp" e3)
  (load-file "CLOS/add-remove-direct-method-support.lisp" e3)
  (load-file "CLOS/add-remove-direct-method-defmethods.lisp" e3))

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

(defun define-ensure-class (e3)
  (load-file "CLOS/ensure-class-using-class-support.lisp" e3)
  ;; When we loaded the support code, we defined
  ;; FIND-SUPERCLASS-OR-NIL to call FIND-CLASS, but that's wrong
  ;; during bootstrapping, so we redifine it here.
  (setf (sicl-genv:fdefinition 'sicl-clos::find-superclass-or-nil e3)
        (lambda (name)
          (sicl-genv:find-class name e3)))
  ;; Uncomment this code once we have the class NULL.
  ;; (load-file "CLOS/ensure-class-using-class-defgenerics.lisp" e3)
  ;; (load-file "CLOS/ensure-class-using-class-defmethods.lisp" e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e3)
        (lambda (&rest arguments)
          (apply (sicl-genv:fdefinition
                  'sicl-clos::ensure-class-using-class-null e3)
                 arguments))))

(defun enable-class-initialization-in-e3 (e2 e3 e4)
  (import-functions-from-host '(sicl-genv:typep) e3)
  (setf (sicl-genv:fdefinition 'typep e3)
        (lambda (object type)
          (sicl-genv:typep object type e3)))
  (define-validate-superclass e3)
  (define-direct-slot-definition-class e2 e3)
  (define-add-remove-direct-subclass e3)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e3 t) nil)
  (define-add-remove-method e3)
  (load-file-protected "CLOS/add-accessor-method.lisp" e3)
  (define-find-or-create-generic-function e3 e4)
  (load-file "CLOS/default-superclasses-defgeneric.lisp" e3)
  (load-file "CLOS/default-superclasses-defmethods.lisp" e3)
  (load-file "CLOS/class-initialization-support.lisp" e3)
  (load-file "CLOS/class-initialization-defmethods.lisp" e3)
  (setf (sicl-genv:fdefinition '(setf find-class) e3)
        (lambda (new-class symbol &optional errorp)
          (declare (ignore errorp))
          (setf (sicl-genv:find-class symbol e3) new-class)))
  (define-error-function 'change-class e3)
  (define-error-function 'reinitialize-instance e3)
  (define-ensure-class e3)
  (import-function-from-host 'sicl-clos:defclass-expander e3)
  (load-file "CLOS/defclass-defmacro.lisp" e3)
  (import-function-from-host '(setf sicl-genv:special-variable) e3)
  (define-error-function 'slot-value e3)
  (define-error-function '(setf slot-value) e3)
  (import-functions-from-host
   '(equal set-exclusive-or sicl-genv:find-class)
   e3)
  ;; FIXME: load files containing the definition instead.
  (setf (sicl-genv:fdefinition 'sicl-clos:add-direct-method e3)
        (constantly nil))
  (define-dependent-protocol e3)
  (define-reader/writer-method-class e2 e3))
