(cl:in-package #:sicl-structure)

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

(define-condition slot-name-must-be-non-nil-symbol (type-error program-error)
  ()
  (:default-initargs :expected-type '(and symbol (not null))))

(define-condition included-structure-name-must-name-structure-type (program-error)
  ((%structure-name :initarg :structure-name :reader structure-name)))

(define-condition structure-name-must-be-non-nil-symbol (type-error program-error)
  ()
  (:default-initargs :expected-type '(and symbol (not null))))
