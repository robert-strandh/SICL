(cl:in-package #:sicl-boot-phase-5)

(defun create-mop-classes (boot)
  (with-accessors ((e5 sicl-boot:e5))
      boot
    (import-functions-from-host
     '(cleavir-code-utilities:proper-list-p
       atom
       eq
       not
       cdr
       (setf cdr)
       rplacd
       +
       member
       cddr
       symbolp)
     e5)
    (load-fasl "CLOS/t-defclass.fasl" e5)
    (setf (sicl-genv:special-variable 'sicl-clos::*class-t* e5 t)
          (sicl-genv:find-class 't e5))
    (load-fasl "CLOS/function-defclass.fasl" e5)
    (load-fasl "CLOS/standard-object-defclass.fasl" e5)
    (load-fasl "CLOS/metaobject-defclass.fasl" e5)
    (load-fasl "CLOS/method-defclass.fasl" e5)
    (load-fasl "CLOS/standard-method-defclass.fasl" e5)
    (load-fasl "CLOS/standard-accessor-method-defclass.fasl" e5)
    (load-fasl "CLOS/standard-reader-method-defclass.fasl" e5)
    (load-fasl "CLOS/standard-writer-method-defclass.fasl" e5)
    (load-fasl "CLOS/slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/standard-slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/direct-slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/effective-slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/standard-direct-slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/standard-effective-slot-definition-defclass.fasl" e5)
    (load-fasl "CLOS/method-combination-defclass.fasl" e5)
    (load-fasl "CLOS/specializer-defclass.fasl" e5)
    (load-fasl "CLOS/eql-specializer-defclass.fasl" e5)
    (load-fasl "CLOS/class-unique-number-defparameter.fasl" e5)
    (load-fasl "CLOS/class-defclass.fasl" e5)
    (load-fasl "CLOS/forward-referenced-class-defclass.fasl" e5)
    (load-fasl "CLOS/real-class-defclass.fasl" e5)
    (load-fasl "CLOS/regular-class-defclass.fasl" e5)
    (load-fasl "CLOS/standard-class-defclass.fasl" e5)
    (load-fasl "CLOS/funcallable-standard-class-defclass.fasl" e5)
    (load-fasl "CLOS/built-in-class-defclass.fasl" e5)
    (load-fasl "CLOS/funcallable-standard-object-defclass.fasl" e5)
    (load-fasl "CLOS/generic-function-defclass.fasl" e5)
    (load-fasl "CLOS/standard-generic-function-defclass.fasl" e5)
    (load-fasl "Cons/cons-defclass.fasl" e5)
    (load-fasl "Sequence/sequence-defclass.fasl" e5)
    (load-fasl "Cons/list-defclass.fasl" e5)
    (load-fasl "Package-and-symbol/symbol-defclass.fasl" e5)
    (load-fasl "Arithmetic/number-defclass.fasl" e5)
    (load-fasl "Arithmetic/real-defclass.fasl" e5)
    (load-fasl "Arithmetic/rational-defclass.fasl" e5)
    (load-fasl "Arithmetic/integer-defclass.fasl" e5)
    (load-fasl "Arithmetic/fixnum-defclass.fasl" e5)))
