(cl:in-package #:common-lisp-user)

(defpackage #:sicl-run-time
  (:use #:common-lisp)
  (:shadow #:symbol-value
           #:makunbound
           #:boundp
           #:throw)
  (:export #:*dynamic-environment*
           #:block/tagbody-entry
           #:stack-pointer
           #:frame-pointer
           #:catch-entry
           #:tag
           #:throw-function
           #:special-variable-entry
           #:identifier
           #:name
           #:value
           #:unwind-protect-entry
           #:thunk
           #:invalidate-entry
           #:augment-with-block/tagbody-entry
           #:augment-with-catch-entry
           #:augment-with-special-variable-entry
           #:unwind
           #:initialize-values
           #:call-with-values
           #:find-special-variable-entry
           #:symbol-value
           #:makunbound
           #:boundp
           #:throw
           #:*code-vector*
           #:*literals-vector*
           #:resolve-load-time-value))
