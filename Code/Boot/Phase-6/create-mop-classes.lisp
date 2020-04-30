(cl:in-package #:sicl-boot-phase-6)

(defun create-mop-classes (ea)
  (load-fasl "CLOS/t-defclass.fasl" ea)
  (setf (sicl-genv:special-variable 'sicl-clos::*class-t* ea t)
        (sicl-genv:find-class 't ea))
  (load-fasl "CLOS/function-defclass.fasl" ea)
  (load-fasl "CLOS/standard-object-defclass.fasl" ea)
  (load-fasl "CLOS/metaobject-defclass.fasl" ea)
  (load-fasl "CLOS/method-defclass.fasl" ea)
  (load-fasl "CLOS/standard-method-defclass.fasl" ea)
  (load-fasl "CLOS/standard-accessor-method-defclass.fasl" ea)
  (load-fasl "CLOS/standard-reader-method-defclass.fasl" ea)
  (load-fasl "CLOS/standard-writer-method-defclass.fasl" ea)
  (load-fasl "CLOS/slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/standard-slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/direct-slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/effective-slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/standard-direct-slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/standard-effective-slot-definition-defclass.fasl" ea)
  (load-fasl "CLOS/method-combination-defclass.fasl" ea)
  (load-fasl "CLOS/specializer-defclass.fasl" ea)
  (load-fasl "CLOS/eql-specializer-defclass.fasl" ea)
  (load-fasl "CLOS/class-unique-number-defparameter.fasl" ea)
  (load-fasl "CLOS/class-defclass.fasl" ea)
  (load-fasl "CLOS/forward-referenced-class-defclass.fasl" ea)
  (load-fasl "CLOS/real-class-defclass.fasl" ea)
  (load-fasl "CLOS/regular-class-defclass.fasl" ea)
  (load-fasl "CLOS/standard-class-defclass.fasl" ea)
  (load-fasl "CLOS/funcallable-standard-class-defclass.fasl" ea)
  (load-fasl "CLOS/built-in-class-defclass.fasl" ea)
  (load-fasl "CLOS/funcallable-standard-object-defclass.fasl" ea)
  (load-fasl "CLOS/simple-function-defclass.fasl" ea)
  (load-fasl "CLOS/generic-function-defclass.fasl" ea)
  (load-fasl "CLOS/standard-generic-function-defclass.fasl" ea)
  (load-fasl "Cons/cons-defclass.fasl" ea)
  (load-fasl "Sequence/sequence-defclass.fasl" ea)
  (load-fasl "Cons/list-defclass.fasl" ea)
  (load-fasl "Package-and-symbol/symbol-defclass.fasl" ea)
  (load-fasl "Cons/null-defclass.fasl" ea)
  (load-fasl "Arithmetic/number-defclass.fasl" ea)
  (load-fasl "Arithmetic/real-defclass.fasl" ea)
  (load-fasl "Arithmetic/rational-defclass.fasl" ea)
  (load-fasl "Arithmetic/integer-defclass.fasl" ea)
  (load-fasl "Arithmetic/fixnum-defclass.fasl" ea)
  (load-fasl "Character/character-defclass.fasl" ea)
  (load-fasl "Compiler/Code-object/code-object-defclass.fasl" ea))
