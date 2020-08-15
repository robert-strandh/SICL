(cl:in-package #:sicl-structure)

;;; Parser errors.

(define-condition invalid-option-name (program-error)
  ((%option-name :initarg :option-name :reader option-name)))

(define-condition duplicate-option (program-error)
  ((%option-name :initarg :option-name :reader option-name)))

(define-condition malformed-option (program-error)
  ((%option :initarg :option :reader option)))

(define-condition conc-name-must-be-string-designator (type-error program-error)
  ()
  (:default-initargs :expected-type 'string-designator))

(define-condition duplicate-name (program-error)
  ((%option :initarg :option :reader option)))

(define-condition name-must-be-symbol (type-error program-error)
  ((%option :initarg :option :reader option))
  (:default-initargs :expected-type 'symbol))

(define-condition predicate-requires-named-structure (program-error)
  ())

(define-condition initial-offset-requires-typed-structure (program-error)
  ())

(define-condition print-object/function-requires-structure-object-structure (program-error)
  ())

(define-condition print-object-and-print-function-mutually-exclusive (program-error)
  ())

(define-condition duplicate-slot-option (program-error)
  ((%option-name :initarg :option-name :reader option-name)
   (%slot-description :initarg :slot-description :reader slot-description)))

(define-condition duplicate-slot (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)))

(define-condition malformed-slot-description (program-error)
  ((%slot-description :initarg :slot-description :reader slot-description)))

(define-condition slot-name-must-be-symbol (type-error program-error)
  ()
  (:default-initargs :expected-type 'symbol))

(define-condition included-structure-name-must-name-structure-type (program-error)
  ((%structure-name :initarg :structure-name :reader structure-name)))

(define-condition structure-name-must-be-non-nil-symbol (type-error program-error)
  ()
  (:default-initargs :expected-type '(and symbol (not null))))

;;; Common errors while expanding defstructs.

(define-condition included-structure-does-not-exist (program-error)
  ((%name :initarg :name :reader name)))

(define-condition included-slot-missing-from-parent (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)))

(define-condition included-slot-conflicts-with-parent-slot (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)
   (%parent-slot-name :initarg :parent-slot-name :reader parent-slot-name)))

(define-condition direct-slot-conflicts-with-parent-slot (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)
   (%parent-slot-name :initarg :parent-slot-name :reader parent-slot-name)))

(define-condition included-slot-must-be-read-only (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)))

;;; Errors while expanding structure-object defstructs.

(define-condition included-structure-must-not-be-typed (program-error)
  ((%name :initarg :name :reader name)))

(define-condition included-structure-must-be-structure (type-error program-error)
  ((%name :initarg :name :reader name))
  (:default-initargs :expected-type 'structure-class))

;;; Errors while expanding typed defstructs

(define-condition invalid-defstruct-type (program-error)
  ((%type :initarg :type :reader structure-type)))

(define-condition included-structure-must-be-typed (program-error)
  ((%name :initarg :name :reader name)))

(define-condition included-structure-type-is-incompatible (program-error)
  ((%type :initarg :type :reader structure-type)
   (%included-type :initarg :included-type :reader included-structure-type)))

(define-condition included-slot-type-must-be-subtype (program-error)
  ((%slot-name :initarg :slot-name :reader slot-name)
   (%type :initarg :type :reader slot-type)
   (%included-type :initarg :included-type :reader included-slot-type)))

;;; #S reader errors

(define-condition non-empty-list-must-follow-sharp-s (reader-error)
  ())

(define-condition missing-sharp-s-argument (reader-error)
  ())

(define-condition sharp-s-class-must-name-structure-class (reader-error)
  ((%name :initarg :name :reader name)))

(define-condition sharp-s-class-must-have-standard-constructor (reader-error)
  ((%name :initarg :name :reader name)))

;;; Other errors

(define-condition slot-is-read-only ()
  ((%slot-name :initarg :slot-name :reader slot-name)
   (%object :initarg :object :reader object)))

(define-condition undefined-structure-description ()
  ((%name :initarg :name :reader name)))
