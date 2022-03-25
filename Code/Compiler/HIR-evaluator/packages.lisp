(cl:in-package #:common-lisp-user)

(defpackage #:sicl-hir-evaluator
  (:use #:common-lisp)
  (:local-nicknames (#:env #:sicl-environment))
  (:export #:top-level-hir-to-host-function
           #:call-stack-entry
           #:origin
           #:arguments
           #:*call-stack*
           #:with-new-call-stack-entry
           #:enclose
           #:initialize-closure
           #:fill-environment
           #:instruction-thunk
           #:make-thunk
           #:input
           #:output
           #:successor
           #:lexical-value
           #:input-value))
