(cl:in-package #:common-lisp-user)

(defpackage #:sicl-loop
  (:use #:common-lisp
        #:sicl-additional-conditions)
  (:shadow
   ;; We use TYPE as an accessor for a TYPE-SPEC so we need to shadow
   ;; this name.
   #:type
   ;; We use CONDITION as an accessor for a conditional clause so we
   ;; need to shadow this name
   #:condition
   )
  (:export #:define-parser
           #:expand-body
           #:clause
           #:subclauses-mixin
           #:var-and-type-spec-mixin
           #:compound-forms-mixin
           #:loop-return-clause-mixin))
