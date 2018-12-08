(cl:in-package #:sicl-clos)

(define-condition class-name-must-be-non-nil-symbol
    (sicl-additional-conditions:sicl-type-error)
  ()
  (:default-initargs :type '(and symbol (not null))))

(define-condition attempt-to-add-existing-subclass
    (sicl-additional-conditions:sicl-error)
  ((%subclass :initarg :subclass :reader subclass)
   (%superclass :initarg :superclass :reader superclass)))

(define-condition readers-must-be-proper-list
    (sicl-additional-conditions:sicl-type-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition)
   (%readers :initarg :readers :reader readers))
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option
    (sicl-additional-conditions:sicl-type-error)
  ()
  (:default-initargs :type 'list))

(define-condition attempt-to-access-prototype-of-unfinalized-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-precedence-list-of-unfinalized-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-default-initargs-of-unfinalized-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-effective-slots-of-unfinalized-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-precedence-list-of-forward-referenced-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-default-initargs-of-forward-referenced-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-effective-slots-of-forward-referenced-class
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition malformed-specializer
    (sicl-additional-conditions:sicl-error)
  ((%specializer :initarg :specializer :reader specializer)))

(define-condition no-such-class-name
    (sicl-additional-conditions:sicl-type-error)
  ()
  (:default-initargs :type 'symbol))

(define-condition slot-definition-argument-must-be-supplied
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition unable-to-compute-class-precedence-list
    (sicl-additional-conditions:sicl-error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition option-or-method-must-be-non-empty-list
    (sicl-additional-conditions:sicl-error)
  ((%expression :initarg :expression :reader expressions)))

(define-condition method-already-associated-with-a-generic-function
    (sicl-additional-conditions:sicl-error)
  ((%method-to-add :initarg :method-to-add :reader method-to-add)
   (%its-generic-function :initarg :its-generic-function
                          :reader its-generic-function)))

(define-condition direct-default-initargs-must-be-a-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%initargs :initarg :initargs :reader initargs)))

(define-condition direct-default-initarg-must-be-a-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition direct-default-initarg-must-be-a-list-of-three-elements
    (sicl-additional-conditions:sicl-error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition name-of-direct-default-initarg-must-be-a-symbol
    (sicl-additional-conditions:sicl-error)
  ((%initarg :initarg :initarg :reader initarg)
   (%name :name :name :reader name)))

(define-condition third-element-of-direct-default-initarg-must-be-a-thunk
    (sicl-additional-conditions:sicl-error)
  ((%initarg :initarg :initarg :reader initarg)
   (%initfunction :initarg :initfunction :reader initfunction)))

(define-condition direct-superclasses-must-be-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%superclasses :initarg :superclasses :reader superclasses)))

(define-condition superclass-must-be-a-class-metaobject
    (sicl-additional-conditions:sicl-error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-superclass-must-be-a-class-metaobject-or-a-symbol
    (sicl-additional-conditions:sicl-error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition superclass-not-valid-for-class
    (sicl-additional-conditions:sicl-error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-slots-must-be-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%direct-lost :initarg :direct-lost :reader direct-lost)))

(define-condition qualifier-must-be-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%qualifier :initarg :qualifier :reader qualifier)))

(define-condition argument-precedence-order-must-be-proper-list
    (sicl-additional-conditions:sicl-error)
  ((%order :initarg :order :reader argument-precedence-order)))

(define-condition no-such-generic-function-class
    (sicl-additional-conditions:sicl-error)
  ((%class-name :initarg :class-name :reader generic-function-class-name)))
