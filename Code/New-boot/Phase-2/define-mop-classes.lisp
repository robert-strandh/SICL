(cl:in-package #:sicl-new-boot-phase-2)

(defun define-ensure-class (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2))
      boot
    (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class e2)
          (lambda (name
                   &rest keys
                   &key
                     direct-default-initargs
                     direct-slots
                     direct-superclasses
                     (metaclass nil metaclass-p)
                   &allow-other-keys)
            (let* ((metaclass-name (if metaclass-p metaclass 'standard-class))
                   (metaclass-class (sicl-genv:find-class metaclass-name e1))
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
                                       collect (sicl-genv:find-class name e2)))
                   (remaining-keys (copy-list keys)))
              (loop while (remf remaining-keys :metaclass))
              (loop while (remf remaining-keys :direct-superclasses))
              (setf (sicl-genv:find-class name e2)
                    (apply #'make-instance metaclass-class
                           :direct-default-initargs direct-default-initargs
                           :direct-slots direct-slots
                           :direct-superclasses superclasses
                           :name name
                           remaining-keys)))))))

(defun create-mop-classes (boot)
  (with-accessors ((e2 sicl-new-boot:e2))
      boot
    (import-function-from-host 'sicl-clos:defclass-expander e2)
    (load-file "CLOS/defclass-defmacro.lisp" e2)
    (define-ensure-class boot)
    (load-file "CLOS/t-defclass.lisp" e2)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e2 t)
          (sicl-genv:find-class 't e2))
    (load-file "CLOS/function-defclass.lisp" e2)
    (load-file "CLOS/standard-object-defclass.lisp" e2)
    (load-file "CLOS/metaobject-defclass.lisp" e2)
    (load-file "CLOS/method-defclass.lisp" e2)
    (load-file "CLOS/standard-method-defclass.lisp" e2)
    (load-file "CLOS/standard-accessor-method-defclass.lisp" e2)
    (load-file "CLOS/standard-reader-method-defclass.lisp" e2)
    (load-file "CLOS/standard-writer-method-defclass.lisp" e2)
    (load-file "CLOS/slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/direct-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/effective-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-direct-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/standard-effective-slot-definition-defclass.lisp" e2)
    (load-file "CLOS/method-combination-defclass.lisp" e2)
    (load-file "CLOS/specializer-defclass.lisp" e2)
    (load-file "CLOS/eql-specializer-defclass.lisp" e2)
    (load-file "CLOS/class-unique-number-defparameter.lisp" e2)
    (load-file "CLOS/class-defclass.lisp" e2)
    (load-file "CLOS/forward-referenced-class-defclass.lisp" e2)
    (load-file "CLOS/real-class-defclass.lisp" e2)
    (load-file "CLOS/regular-class-defclass.lisp" e2)
    (load-file "CLOS/standard-class-defclass.lisp" e2)
    (load-file "CLOS/funcallable-standard-class-defclass.lisp" e2)
    (load-file "CLOS/built-in-class-defclass.lisp" e2)
    (load-file "CLOS/funcallable-standard-object-defclass.lisp" e2)
    (load-file "CLOS/generic-function-defclass.lisp" e2)
    (load-file "CLOS/standard-generic-function-defclass.lisp" e2)
    (load-file "Cons/cons-defclass.lisp" e2)
    (load-file "Sequences/sequence-defclass.lisp" e2)
    (load-file "Cons/list-defclass.lisp" e2)
    (load-file "Package-and-symbol/symbol-defclass.lisp" e2)))
