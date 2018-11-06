(cl:in-package #:sicl-new-boot-phase-3)

(defmethod sicl-genv:typep
    (object (type-specifier (eql 'function)) (environment environment))
  (typep object 'function))

;;; We only check for the type CLASS for reasons of catching errors,
;;; but during bootstrapping, we completely control the arguments, so
;;; we can simply return true here.
(defmethod sicl-genv:typep
    (object (type-specifier (eql 'class)) (environment environment))
  (not (symbolp object)))

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

(defun create-mop-classes (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4))
      boot
    (load-file "CLOS/t-defclass.lisp" e3)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e2 t)
          (sicl-genv:find-class 't e3))
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
    (load-file "CLOS/standard-generic-function-defclass.lisp" e3)
    (load-file "Cons/cons-defclass.lisp" e3)
    (load-file "Sequences/sequence-defclass.lisp" e3)
    (load-file "Cons/list-defclass.lisp" e3)
    (load-file "Package-and-symbol/symbol-defclass.lisp" e3)))
