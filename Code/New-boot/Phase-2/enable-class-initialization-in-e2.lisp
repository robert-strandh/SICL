(cl:in-package #:sicl-new-boot-phase-2)

(defun define-add-remove-direct-subclass (e2)
  ;; REMOVE is called by REMOVE-DIRECT-SUBCLASS in order to remove a
  ;; class from the list of subclasses of some class.
  (import-function-from-host 'remove e2)
  (load-file "CLOS/add-remove-direct-subclass-support.lisp" e2)
  (load-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e2)
  (load-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e2))

(defun define-add-remove-method (e2)
  (load-file "CLOS/add-remove-method-defgenerics.lisp" e2)
  ;; MAKE-LIST is called when a method is removed and a new
  ;; specializer profile must be computed.
  (import-function-from-host 'make-list e2)
  ;; FIND-IF is called by ADD-METHOD in order to find and existing
  ;; method with the same specializers and the same qualifiers, so
  ;; that that existing method can be removed first.
  (import-function-from-host 'find-if e2)
  (load-file-protected "CLOS/add-remove-method-support.lisp" e2)
  (load-file "CLOS/add-remove-method-defmethods.lisp" e2))

(defun define-add-remove-direct-method (e2)
  ;; ADJOIN is called by ADD-DIRECT-METHOD.
  ;; REMOVE is called by REMOVE-DIRECT-METHOD.
  (import-functions-from-host '(adjoin remove) e2)
  (load-file "CLOS/add-remove-direct-method-defgenerics.lisp" e2)
  (load-file "CLOS/add-remove-direct-method-support.lisp" e2)
  (load-file "CLOS/add-remove-direct-method-defmethods.lisp" e2))

(defun define-reader/writer-method-class (e1 e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:reader-method-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-reader-method e1)))
  (setf (sicl-genv:fdefinition 'sicl-clos:writer-method-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-writer-method e1))))

(defun define-direct-slot-definition-class (e1 e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:direct-slot-definition-class e2)
        (lambda (&rest arguments)
          (declare (ignore arguments))
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e1))))

(defun define-find-or-create-generic-function (e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos::find-or-create-generic-function e2)
        (lambda (name lambda-list)
          (declare (ignore lambda-list))
          (sicl-genv:fdefinition name e3))))

(defun enable-class-initialization-in-e2 (e1 e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:validate-superclass e2)
        (constantly t))
  (define-direct-slot-definition-class e1 e2)
  (define-add-remove-direct-subclass e2)
  (define-reader/writer-method-class e1 e2)
  (setf (sicl-genv:fdefinition 'compile e2)
        (lambda (name &optional definition)
          (assert (null name))
          (assert (not (null definition)))
          (cleavir-env:eval definition e2 e2)))
  (import-functions-from-host '(adjoin set-exclusive-or) e2)
  (define-error-function 'slot-value e2)
  (define-error-function '(setf slot-value) e2)
  (define-add-remove-direct-method e2)
  (setf (sicl-genv:fdefinition 'sicl-clos:map-dependents e2)
        (lambda (&rest args) (declare (ignore args)) nil))
  (setf (sicl-genv:fdefinition 'sicl-clos:update-dependent e2)
        (lambda (&rest args) (declare (ignore args)) nil))
  (define-add-remove-method e2)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e2 nil) nil)
  (load-file "CLOS/add-accessor-method.lisp" e2)
  (define-find-or-create-generic-function e2 e3)
  (setf (sicl-genv:fdefinition 'sicl-clos:default-superclasses e2)
        (lambda (class) (declare (ignore class)) '()))
  (import-function-from-host 'sicl-genv:typep e2)
  (load-file "CLOS/class-initialization-support.lisp" e2)
  (load-file "CLOS/class-initialization-defmethods.lisp" e2)
  (sicl-genv:fmakunbound 'shared-initialize e2))
