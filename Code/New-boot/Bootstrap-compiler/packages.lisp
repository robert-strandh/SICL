(cl:in-package #:common-lisp-user)

(defpackage #:sicl-new-boot-bootstrap-compiler
  (:use #:common-lisp)
  (:local-nicknames (#:cb #:common-boot)
                    (#:cbfe #:common-boot-fast-ast-evaluator)
                    (#:abp #:architecture.builder-protocol)
                    (#:clo #:clostrum))
  (:export))
