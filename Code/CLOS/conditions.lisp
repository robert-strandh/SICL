(cl:in-package #:sicl-clos)

(define-condition class-name-must-be-non-nil-symbol
    (type-error)
  ()
  (:default-initargs :type '(and symbol (not null))))

(define-condition attempt-to-add-existing-subclass
    (error)
  ((%subclass :initarg :subclass :reader subclass)
   (%superclass :initarg :superclass :reader superclass)))

(define-condition readers-must-be-proper-list
    (type-error)
  ((%slot-definition :initarg :slot-definition :reader slot-definition)
   (%readers :initarg :readers :reader readers))
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option
    (type-error)
  ()
  (:default-initargs :type 'list))

(define-condition attempt-to-access-prototype-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-precedence-list-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-default-initargs-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-effective-slots-of-unfinalized-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-precedence-list-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-default-initargs-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition attempt-to-access-effective-slots-of-forward-referenced-class
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition malformed-specializer
    (error)
  ((%specializer :initarg :specializer :reader specializer)))

(define-condition no-such-class-name
    (error)
  ((%name :initarg :name :reader name)))

(define-condition slot-definition-argument-must-be-supplied
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition unable-to-compute-class-precedence-list
    (error)
  ((%offending-class :initarg :offending-class :reader offending-class)))

(define-condition option-or-method-must-be-non-empty-list
    (error)
  ((%expression :initarg :expression :reader expressions)))

(define-condition method-already-associated-with-a-generic-function
    (error)
  ((%method-to-add :initarg :method-to-add :reader method-to-add)
   (%its-generic-function :initarg :its-generic-function
                          :reader its-generic-function)))

(define-condition direct-default-initargs-must-be-a-proper-list
    (error)
  ((%initargs :initarg :initargs :reader initargs)))

(define-condition direct-default-initarg-must-be-a-proper-list
    (error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition direct-default-initarg-must-be-a-list-of-three-elements
    (error)
  ((%initarg :initarg :initarg :reader initarg)))

(define-condition name-of-direct-default-initarg-must-be-a-symbol
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%name :initarg :name :reader name)))

(define-condition third-element-of-direct-default-initarg-must-be-a-thunk
    (error)
  ((%initarg :initarg :initarg :reader initarg)
   (%initfunction :initarg :initfunction :reader initfunction)))

(define-condition direct-superclasses-must-be-proper-list
    (error)
  ((%superclasses :initarg :superclasses :reader superclasses)))

(define-condition superclass-must-be-a-class-metaobject
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-superclass-must-be-a-class-metaobject-or-a-symbol
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition superclass-not-valid-for-class
    (error)
  ((%superclass :initarg :superclass :reader superclass)))

(define-condition direct-slots-must-be-proper-list
    (error)
  ((%direct-lost :initarg :direct-lost :reader direct-lost)))

(define-condition qualifier-must-be-proper-list
    (error)
  ((%qualifier :initarg :qualifier :reader qualifier)))

(define-condition argument-precedence-order-must-be-proper-list
    (error)
  ((%order :initarg :order :reader argument-precedence-order)))

(define-condition no-such-generic-function-class
    (error)
  ((%class-name :initarg :class-name :reader generic-function-class-name)))

(define-condition no-such-method-class
    (error)
  ((%class-name :initarg :class-name :reader generic-function-class-name)))

(define-condition generic-function-class-must-be-class-or-name
    (error)
  ((%object :initarg :object :reader object)))
