(cl:in-package #:sicl-boot-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating MOP classes.
;;;
;;; The purpose of this step of phase 1 is to create a class hierarchy
;;; corresponding to the MOP classes.  These classes will all be host
;;; funallable standard classes.

;;; This function defines a version of ENSURE-CLASS to be used in
;;; phase 1.  The definition of ENSURE-CLASS is made in E1.  This
;;; version of ENSURE-CLASS defines a new class in E1.  It also uses
;;; E1 to find superclasses of the class to be defined.  E2 is the
;;; environment containing the generic functions to which slot reader
;;; and slot writer methods are to be added.  E0 is the environment
;;; that contains the metaclasses to instantiate.
(defun define-ensure-class (boot)
  (with-accessors ((e0 sicl-boot:e0)
                   (e1 sicl-boot:e1)
                   (e2 sicl-boot:e2))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e1)
          (lambda (class-name
                   &key
                     direct-slots
                     ((:direct-superclasses direct-superclass-names))
                     name
                     ((:metaclass metaclass-name) 'sicl-clos:standard-class)
                   &allow-other-keys)
            ;; We should be called only for the expansion of DEFCLASS,
            ;; and we make sure that we always pass the name of a
            ;; metaclass, and never a class metaobject.
            (assert (symbolp metaclass-name))
            (let (;; In phase 1, the metaclass is a host class to be
                  ;; found in E0.
                  (metaclass (sicl-genv:find-class metaclass-name e0))
                  ;; The direct superclasses, on the other hand, are to
                  ;; be found in the same environment as the one in
                  ;; which this class wil. be defined.
                  (direct-superclasses
                    (loop for name in direct-superclass-names
                          collect (sicl-genv:find-class name e1))))
              (let ((class (make-instance metaclass
                             :name (make-symbol (symbol-name name))
                             :direct-slots direct-slots
                             :direct-superclasses direct-superclasses)))
                (setf (sicl-genv:find-class class-name e1) class)))))))

(defun define-mop-classes-phase1 (boot)
  (with-accessors ((e0 sicl-boot:e0)
                   (e1 sicl-boot:e1)
                   (e2 sicl-boot:e2))
      boot
    (enable-defclass-in-e1 boot)
    (load-file "CLOS/defclass-defmacro.lisp" e1)
    (define-ensure-class boot)
    (load-file "CLOS/t-defclass.lisp" e1)
    (load-file "CLOS/function-defclass.lisp" e1)
    (load-file "CLOS/standard-object-defclass.lisp" e1)
    (load-file "CLOS/metaobject-defclass.lisp" e1)
    (load-file "CLOS/method-defclass.lisp" e1)
    (load-file "CLOS/standard-method-defclass.lisp" e1)
    (load-file "CLOS/standard-accessor-method-defclass.lisp" e1)
    (load-file "CLOS/standard-reader-method-defclass.lisp" e1)
    (load-file "CLOS/standard-writer-method-defclass.lisp" e1)
    (load-file "CLOS/slot-definition-defclass.lisp" e1)
    (load-file "CLOS/standard-slot-definition-defclass.lisp" e1)
    (load-file "CLOS/direct-slot-definition-defclass.lisp" e1)
    (load-file "CLOS/effective-slot-definition-defclass.lisp" e1)
    (load-file "CLOS/standard-direct-slot-definition-defclass.lisp" e1)
    (load-file "CLOS/standard-effective-slot-definition-defclass.lisp" e1)
    (load-file "CLOS/method-combination-defclass.lisp" e1)
    (load-file "CLOS/specializer-defclass.lisp" e1)
    (load-file "CLOS/eql-specializer-defclass.lisp" e1)
    (load-file "CLOS/class-unique-number-defparameter.lisp" e1)
    (load-file "CLOS/class-defclass.lisp" e1)
    (load-file "CLOS/forward-referenced-class-defclass.lisp" e1)
    (load-file "CLOS/real-class-defclass.lisp" e1)
    (load-file "CLOS/regular-class-defclass.lisp" e1)
    (load-file "CLOS/standard-class-defclass.lisp" e1)
    (load-file "CLOS/funcallable-standard-class-defclass.lisp" e1)
    (load-file "CLOS/built-in-class-defclass.lisp" e1)
    (load-file "CLOS/funcallable-standard-object-defclass.lisp" e1)
    (load-file "CLOS/generic-function-defclass.lisp" e1)
    (load-file "CLOS/standard-generic-function-defclass.lisp" e1)
    (load-file "Cons/cons-defclass.lisp" e1)
    (load-file "Sequences/sequence-defclass.lisp" e1)
    (load-file "Cons/list-defclass.lisp" e1)
    (load-file "Package-and-symbol/symbol-defclass.lisp" e1)
    (load-file "Arithmetic/number-defclass.lisp" e1)
    (load-file "Arithmetic/real-defclass.lisp" e1)
    (load-file "Arithmetic/rational-defclass.lisp" e1)
    (load-file "Arithmetic/integer-defclass.lisp" e1)
    (load-file "Arithmetic/fixnum-defclass.lisp" e1)))
