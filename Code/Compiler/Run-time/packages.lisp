(cl:in-package #:common-lisp-user)

(defpackage #:sicl-run-time
  (:use #:common-lisp)
  (:shadow #:symbol-value
           #:makunbound
           #:boundp)
  (:export #:*dynamic-environment*
           #:block/tagbody-entry
           #:stack-pointer
           #:frame-pointer
           #:special-variable-entry
           #:identifier
           #:name
           #:value
           #:unwind-protect-entry
           #:thunk
           #:invalidate-entry
           #:augment-with-block/tagbody-entry
           #:augment-with-special-variable-entry
           #:unwind
           #:initialize-values
           #:call-with-values
           #:find-special-variable-entry
           #:symbol-value
           #:makunbound
           #:boundp))
