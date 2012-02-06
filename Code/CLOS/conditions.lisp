(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition no-such-class-name (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition must-be-class-or-nil (type-error name-mixin)
  ()
  (:default-initargs :type '(or class null)))

(define-condition superclass-list-must-be-proper-list
    (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition class-name-must-be-non-nil-symbol
    (type-error name-mixin)
  ()
  (:default-initargs :type '(and symbol (not null))))

(define-condition malformed-slots-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-slot-spec (type-error name-mixin)
  ()
  (:default-initargs :type '(or symbol list)))

(define-condition illegal-slot-name (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition slot-options-must-be-even (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition slot-option-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition multiple-initform-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-documentation-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-allocation-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-type-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition slot-documentation-option-must-be-string (type-error name-mixin)
  ()
  (:default-initargs :type 'string))

(define-condition class-option-must-be-non-empty-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition class-option-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

;;; FIXME: This doesn't feel like a type-error
(define-condition duplicate-class-option-not-allowed (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition malformed-documentation-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-metaclass-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initargs-must-be-proper-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initargs-must-have-even-length (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initarg-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition malformed-default-initargs-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

