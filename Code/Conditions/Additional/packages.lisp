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
   #:warn-both-test-and-test-not-given
   #:list-as-sequence-must-be-proper
   #:invalid-bounding-indexes
   #:invalid-start-index
   #:invalid-end-index
   #:sicl-program-error
   #:sicl-program-warning
   #:sicl-program-style-warning
   #:multiple-documentation-strings-in-body
   #:documentation-string-not-allowed-in-body
   #:declarations-not-allowed-in-body
   #:declaration-follows-form-in-body
   #:form-too-short
   #:form-too-long
   #:unknown-eval-when-situation
   #:setq-must-have-even-number-arguments
   #:setq-variable-must-be-symbol
   #:tagbody-element-must-be-symbol-integer-or-compound-form
   #:empty-body
   #:unrecognized-keyword-argument
   #:invalid-keyword-argument
   #:odd-number-of-keyword-arguments
   #:sicl-unbound-variable
   #:sicl-unbound-function
   #:class-name-must-be-non-nil-symbol)
  (:shadow #:sequence))
