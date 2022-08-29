(cl:in-package #:common-lisp-user)

(defpackage #:sicl-additional-conditions
  (:use #:common-lisp #:sicl-additional-types)
  (:export
   #:sicl-condition
   #:sicl-warning
   #:sicl-style-warning
   #:sicl-error
   #:sicl-type-error
   #:sicl-cell-error
   #:both-test-and-test-not-given
   #:at-least-one-list-required
   #:at-least-one-argument-required
   #:warn-both-test-and-test-not-given
   #:list-as-sequence-must-be-proper
   #:invalid-bounding-indexes
   #:invalid-start-index
   #:invalid-end-index
   #:sicl-program-error
   #:sicl-program-warning
   #:sicl-program-style-warning
   #:form-must-be-proper-list
   #:body-must-be-proper-list
   #:multiple-documentation-strings-in-body
   #:documentation-string-not-allowed-in-body
   #:declarations-not-allowed-in-body
   #:declaration-follows-form-in-body
   #:form-too-short
   #:form-too-long
   #:unknown-eval-when-situation
   #:go-tag-must-be-symbol-or-integer
   #:setq-must-have-even-number-arguments
   #:setq-variable-must-be-symbol
   #:tagbody-element-must-be-symbol-integer-or-compound-form
   #:empty-body
   #:unrecognized-keyword-argument
   #:invalid-keyword-argument
   #:odd-number-of-keyword-arguments
   #:sicl-unbound-variable
   #:sicl-unbound-function
   #:class-name-must-be-non-nil-symbol
   #:malformed-slots-list
   #:malformed-slot-spec
   #:illegal-slot-name
   #:slot-option-name-must-be-symbol
   #:class-option-must-be-non-empty-list
   #:class-option-name-must-be-symbol
   #:duplicate-class-option-not-allowed
   #:malformed-default-initargs-option
   #:default-initargs-option-once
   #:documentation-option-once
   #:unknown-class-option)
  (:shadow #:sequence))
